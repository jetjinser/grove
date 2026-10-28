;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove ssh)
  #:use-module (grove private string-fun)
  #:use-module (ice-9 format)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 rdelim)
  #:use-module (ssh auth)
  #:use-module (ssh channel)
  #:use-module (ssh key)
  #:use-module (ssh message)
  #:use-module (ssh server)
  #:use-module (ssh session)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (read-authorized-keys
            generate-host-key
            serve-ssh))

;; SSH transport for Grove browsing. It authenticates a client, opens a session
;; channel with a PTY, and hands that channel to a project handler as input and
;; output ports. It never executes a client-supplied command and never parses
;; project sources itself.

(define (authorized-key-blob line)
  "Return the base64 key blob from an authorized-keys LINE, or #f."
  (let loop ((tokens (string-tokenize line)))
    (if (null? tokens)
        #f
        (if (or (string-prefix? "ssh-" (car tokens))
                (string-prefix? "ecdsa-" (car tokens))
                (string-prefix? "sk-" (car tokens)))
            (and (pair? (cdr tokens)) (cadr tokens))
            (loop (cdr tokens))))))

(define (read-authorized-keys path)
  "Read authorized public keys from PATH as base64 blobs."
  (call-with-input-file path
    (lambda (port)
      (let loop ((keys '()))
        (let ((line (read-line port)))
          (if (eof-object? line)
              (reverse keys)
              (let ((trimmed (string-trim-both line)))
                (loop (if (or (string-null? trimmed)
                              (string-prefix? "#" trimmed))
                          keys
                          (let ((blob (authorized-key-blob trimmed)))
                            (if blob (cons blob keys) keys))))))))))) 

(define (public-key-blob pubkey)
  "Return PUBKEY's base64 blob, or #f when it is not a usable public key."
  (catch #t
    (lambda ()
      (and pubkey (public-key? pubkey) (public-key->string pubkey)))
    (lambda _ #f)))

(define (handle-auth msg type authorized output)
  "Answer one authentication message. Return #t once the client is authenticated."
  (message-auth-set-methods! msg '(public-key))
  (case (cadr type)
    ((auth-method-publickey)
     (let* ((request (message-get-req msg))
            (state (catch #t (lambda () (auth-req:pubkey-state request))
                          (lambda _ #f)))
            (offered (public-key-blob
                       (catch #t (lambda () (auth-req:pubkey request))
                              (lambda _ #f)))))
       (case state
         ((none)
          (if (and offered (member offered authorized string=?))
              (message-auth-reply-public-key-ok msg)
              (message-reply-default msg))
          #f)
         ((valid)
          ;; libssh only reports a valid signature; the offered key was already
          ;; checked against the authorized set at the "none" state.
          (message-reply-success msg)
          #t)
         (else
          (message-reply-default msg)
          #f))))
    (else
     (message-reply-default msg)
     #f)))

(define (handle-channel-request msg type channel session handler output)
  "Handle a channel request. Return #t when the session must end."
  (case (cadr type)
    ((channel-request-pty)
     (message-reply-success msg)
     #f)
    ((channel-request-env)
     (message-reply-success msg)
     #f)
    ((channel-request-shell)
     (message-reply-success msg)
     (catch #t
       (lambda () (handler channel channel))
       (lambda (key . arguments)
         (format output "grove ssh: session failed: ~s ~s~%" key arguments)
         #f))
     (catch 'guile-ssh-error
       (lambda () (channel-request-send-exit-status channel 0))
       (lambda _ #t))
     (close-channel channel)
     #t)
    ((channel-request-exec)
     ;; Browsing is the only capability; never run a client command.
     (message-reply-default msg)
     #f)
    (else
     (message-reply-success msg)
     #f)))

(define* (handle-session session handler authorized output
                         #:key (login-timeout 0))
  (let ((deadline (and (> login-timeout 0)
                       (+ (current-time) login-timeout))))
    (let loop ((channel #f) (authenticated #f))
      (cond
        ((and deadline (not authenticated) (>= (current-time) deadline))
         (format output "grove ssh: login timed out after ~a s~%" login-timeout)
         #f)
        (else
         (let ((msg (catch 'guile-ssh-error
                      (lambda () (server-message-get session))
                      (lambda _ #f))))
           (if (not msg)
               (if (connected? session)
                   (begin (usleep 50000) (loop channel authenticated))
                   #f)
               (let ((type (message-get-type msg)))
                 (case (car type)
                   ((request-service)
                    (message-reply-success msg)
                    (loop channel authenticated))
                   ((request-auth)
                    (loop channel (or authenticated
                                      (handle-auth msg type authorized output))))
                   ((request-channel-open)
                    (case (cadr type)
                      ((channel-session)
                       (loop (message-channel-request-open-reply-accept msg)
                             authenticated))
                      (else
                       (message-reply-default msg)
                       (loop channel authenticated))))
                   ((request-channel)
                    (if (handle-channel-request msg type channel session handler
                                                output)
                        #t
                        (loop channel authenticated)))
                   (else
                    (message-reply-default msg)
                    (loop channel authenticated)))))))))))

(define (close-channel channel)
  (when channel
    (catch #t (lambda () (close channel)) (lambda _ #t))))

(define* (generate-host-key path #:key (bits 3072) (type 'rsa) (force #f)
                            (output (current-error-port)))
  "Generate a host key at PATH for use with serve-ssh.

TYPE is a key symbol understood by (ssh key), BITS its size where the
type has one.  Refuses to overwrite an existing key unless FORCE is #t."
  (unless (non-empty-string? path)
    (error "host key requires a file path" path))
  (when (and (file-exists? path) (not force))
    (error "host key already exists; pass --force to overwrite" path))
  (catch 'guile-ssh-error
    (lambda ()
      (let ((key (make-keypair type bits)))
        (private-key-to-file key path)
        (catch #t (lambda () (chmod path #o600)) (lambda _ #t))
        (format output "grove ssh: wrote ~a host key ~a~%"
                (get-key-type key) path)
        path))
    (lambda (key . arguments)
      (error "host key generation failed" type bits key arguments))))

(define (accept-session server)
  (catch 'guile-ssh-error
    (lambda () (server-accept server))
    (lambda _ #f)))

(define (serve-one-session session handler authorized-keys output login-timeout)
  "Authenticate and serve a single SESSION, then disconnect it."
  (server-handle-key-exchange session)
  (catch #t
    (lambda ()
      (handle-session session handler authorized-keys output
                      #:login-timeout login-timeout))
    (lambda (key . arguments)
      (format output "grove ssh: session error: ~s ~s~%" key arguments)))
  (catch 'guile-ssh-error
    (lambda () (disconnect! session))
    (lambda _ #t)))

(define (call-with-session-timeout seconds thunk)
  "Run THUNK, killing the worker if it outlasts SECONDS.

A blocking read cannot be interrupted in place, and the blocking C call holds
the Guile lock, so signals-as-exceptions and watchdog threads cannot run. A
separate watchdog process can: it sends SIGKILL, which cannot be blocked. The
supervisor then restarts the worker."
  (if (not (and seconds (> seconds 0)))
      (thunk)
      (let ((watchdog (primitive-fork)))
        (if (zero? watchdog)
            (let ((worker (getppid)))
              (sleep seconds)
              ;; Only fire while the worker is still our parent, so an orphaned
              ;; watchdog cannot signal a reused PID.
              (when (= (getppid) worker)
                (catch #t (lambda () (kill worker SIGKILL)) (lambda _ #t)))
              (primitive-exit 0))
            (dynamic-wind
              (lambda () #t)
              thunk
              (lambda ()
                (catch #t (lambda () (kill watchdog SIGTERM)) (lambda _ #t))
                (catch #t (lambda () (waitpid watchdog 0)) (lambda _ #t))))))))

(define (worker-main server handler authorized-keys output login-timeout
                     session-timeout memory-limit)
  "Accept and serve sessions until the process is terminated.

MEMORY-LIMIT caps the worker's address space so one session cannot exhaust the
host; SESSION-TIMEOUT bounds a session and is enforced by a watchdog process."
  (when memory-limit
    (catch #t
      (lambda () (setrlimit 'as memory-limit memory-limit))
      (lambda _ #t)))
  (let loop ()
    (let ((session (accept-session server)))
      (if (not session)
          (begin (usleep 50000) (loop))
          (begin
            (catch #t
              (lambda ()
                (call-with-session-timeout
                  session-timeout
                  (lambda ()
                    (serve-one-session session handler authorized-keys output
                                       login-timeout))))
              (lambda (key . arguments)
                (format output "grove ssh: worker session error: ~s ~s~%"
                        key arguments)))
            (loop))))))

(define (spawn-worker server handler authorized-keys output login-timeout
                      session-timeout memory-limit)
  (let ((pid (primitive-fork)))
    (if (zero? pid)
        (begin
          (worker-main server handler authorized-keys output login-timeout
                       session-timeout memory-limit)
          (primitive-exit 0))
        pid)))

(define (reap-dead-children)
  "Reap every exited child without blocking; return the reaped PIDs."
  (let loop ((reaped '()))
    (let ((result (catch #t (lambda () (waitpid -1 WNOHANG))
                         (lambda _ #f))))
      (if (or (not (pair? result)) (zero? (car result)))
          (reverse reaped)
          (loop (cons (car result) reaped))))))

(define (kill-children pids signal)
  (for-each (lambda (pid)
              (catch #t (lambda () (kill pid signal)) (lambda _ #t)))
            pids))

(define (reap-all-children)
  "Block until every child has exited, collecting them to avoid zombies."
  (let loop ()
    (let ((result (catch #t (lambda () (waitpid -1 0)) (lambda _ #f))))
      (when (and (pair? result) (not (zero? (car result))))
        (loop)))))

(define* (serve-ssh #:key
                    (address "127.0.0.1")
                    (port 2222)
                    host-key
                    (authorized-keys '())
                    (banner "Grove")
                    (log-verbosity 'nolog)
                    (login-timeout 60)
                    (session-timeout 0)
                    (workers 4)
                    memory-limit
                    handler
                    (output (current-error-port)))
  "Serve HANDLER over SSH until interrupted.

HANDLER receives the session channel as both input and output. HOST-KEY is an
RSA host key file; AUTHORIZED-KEYS is a list from read-authorized-keys. The
default listener is 127.0.0.1:2222; PORT must be from 1 to 65535.

WORKERS is the number of session-handling processes, and thus the concurrency
limit. LOGIN-TIMEOUT drops clients that do not authenticate in time;
SESSION-TIMEOUT caps a browsing session (0 disables it); MEMORY-LIMIT, in bytes,
caps each worker's address space."
  (unless (procedure? handler)
    (error "SSH server requires a session handler" handler))
  (unless (non-empty-string? address)
    (error "SSH server address must be a non-empty string" address))
  (unless (and (exact-integer? port) (<= 1 port 65535))
    (error "SSH server port must be an integer from 1 to 65535" port))
  (unless (and (list? authorized-keys)
               (every non-empty-string? authorized-keys))
    (error "SSH authorized keys must be a list of non-empty key blobs"
           authorized-keys))
  (unless (non-empty-string? banner)
    (error "SSH server banner must be a non-empty string" banner))
  (unless (and (string? host-key) (file-exists? host-key))
    (error "SSH server requires an existing host key" host-key))
  (unless (and (exact-integer? login-timeout) (>= login-timeout 0))
    (error "SSH login timeout must be a non-negative integer" login-timeout))
  (unless (and (exact-integer? session-timeout) (>= session-timeout 0))
    (error "SSH session timeout must be a non-negative integer" session-timeout))
  (unless (and (exact-integer? workers) (<= 1 workers 64))
    (error "SSH worker count must be an integer from 1 to 64" workers))
  (unless (or (not memory-limit)
              (and (exact-integer? memory-limit) (> memory-limit 0)))
    (error "SSH memory limit must be a positive integer or #f" memory-limit))
  (unless (output-port? output)
    (error "SSH server output must be an output port" output))
  (let ((server (make-server #:bindaddr address
                             #:bindport port
                             #:rsakey host-key
                             #:banner banner
                             #:log-verbosity log-verbosity)))
    (define (spawn)
      (spawn-worker server handler authorized-keys output
                    login-timeout session-timeout memory-limit))
    (server-listen server)
    (let ((children (map (lambda (_) (spawn)) (iota workers))))
      (define (terminate status)
        (kill-children children SIGTERM)
        (reap-all-children)
        (primitive-exit status))
      (sigaction SIGINT (lambda (_signal) (terminate 130)))
      (sigaction SIGTERM (lambda (_signal) (terminate 143)))
      (format output "grove ssh: serving on ~a:~a with ~a worker(s)~%"
              address port workers)
      (dynamic-wind
        (lambda () #t)
        (lambda ()
          ;; Supervise: restart any worker that exits so the pool stays full.
          (let supervise ((pids children))
            (set! children pids)
            (let* ((dead (reap-dead-children))
                   (alive (remove (lambda (pid) (memq pid dead)) pids))
                   (fresh (map (lambda (_) (spawn)) dead))
                   (next (append fresh alive)))
              (set! children next)
              (usleep 200000)
              (supervise next))))
        (lambda ()
          ;; Kill workers on any exit, including an uncaught error, so they do
          ;; not keep the listening socket.
          (kill-children children SIGTERM)
          (reap-all-children))))))
