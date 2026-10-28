;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove ssh)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64))

;; SSH transport tests. (grove ssh) is a conditional component, so it is
;; resolved at run time: when guile-ssh is absent these tests skip instead of
;; failing to compile. The parser and host-key generation are covered directly;
;; the transport forks a server and connects with ssh(1) wrapped in timeout(1),
;; skipping when those tools are unavailable.

(define ssh-module
  (catch #t
    (lambda () (resolve-module '(grove ssh) #:ensure #f))
    (lambda _ #f)))

(define (ssh-proc name) (module-ref ssh-module name))
(define ssh-supported? (and ssh-module #t))

(define test-directory
  (mkdtemp (string-append (or (getenv "TMPDIR") "/tmp")
                          "/grove-ssh-test-XXXXXX")))

(define (have-program? program)
  (and=> (search-path (string-split (or (getenv "PATH") "") #\:) program)
         (lambda (_path) #t)))

(define (run program . arguments)
  (catch #t
    (lambda () (zero? (status:exit-val (apply system* program arguments))))
    (lambda _ #f)))

(define ssh-keygen? (have-program? "ssh-keygen"))

(define host-key (string-append test-directory "/host_rsa"))
(define client-key (string-append test-directory "/client_ed25519"))
(define other-key (string-append test-directory "/other_ed25519"))
(define authorized-keys (string-append test-directory "/authorized_keys"))

(define keys-ready?
  (and ssh-keygen?
       (run "ssh-keygen" "-t" "rsa" "-b" "2048" "-m" "PEM" "-N" "" "-q"
            "-f" host-key)
       (run "ssh-keygen" "-t" "ed25519" "-N" "" "-q" "-f" client-key)
       (run "ssh-keygen" "-t" "ed25519" "-N" "" "-q" "-f" other-key)
       (catch #t
         (lambda ()
           (copy-file (string-append client-key ".pub") authorized-keys)
           #t)
         (lambda _ #f))))

(define (free-port)
  "Return a currently free TCP port on the loopback interface."
  (let ((sock (socket AF_INET SOCK_STREAM 0)))
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (bind sock (make-socket-address AF_INET INADDR_LOOPBACK 0))
        (sockaddr:port (getsockname sock)))
      (lambda () (close sock)))))

(define (echo-handler input output)
  (display "ready\n" output)
  (force-output output)
  (let loop ()
    (let ((line (read-line input)))
      (unless (eof-object? line)
        (display (string-append "echo:" line "\n") output)
        (force-output output)
        (loop)))))

(define* (start-server handler #:key (workers 2) (login-timeout 10))
  "Fork a server process; return (pid . port)."
  (let ((port (free-port)))
    (let ((pid (primitive-fork)))
      (if (zero? pid)
          (begin
            ((ssh-proc 'serve-ssh)
              #:port port
              #:host-key host-key
              #:authorized-keys ((ssh-proc 'read-authorized-keys) authorized-keys)
              #:workers workers
              #:login-timeout login-timeout
              #:output (open-output-file "/dev/null")
              #:handler handler)
            (primitive-exit 0))
          (begin (usleep 400000) (cons pid port))))))

(define (stop-server pid)
  (catch #t (lambda () (kill pid SIGTERM)) (lambda _ #t))
  (catch #t (lambda () (waitpid pid 0)) (lambda _ #t)))

(define (ssh-run port key input)
  "Run a short ssh(1) session; return (exit-status . combined-output)."
  (let* ((command
           (string-join
             (list "timeout" "25" "ssh" "-T" "-p" (number->string port)
                   "-i" key
                   "-o" "StrictHostKeyChecking=no"
                   "-o" "UserKnownHostsFile=/dev/null"
                   "-o" "PreferredAuthentications=publickey"
                   "-o" "BatchMode=yes"
                   "-o" "LogLevel=ERROR"
                   "user@127.0.0.1")
             " "))
         (pipe (open-input-pipe
                 (string-append "printf '" input "' | " command " 2>&1")))
         (output (get-string-all pipe))
         (status (close-pipe pipe)))
    (cons (status:exit-val status) output)))

(test-begin "grove ssh")

(if (not ssh-supported?)
    (test-assert "SSH tests skipped: guile-ssh is unavailable" #t)
    (begin
      (test-error "SSH server rejects out-of-range ports" #t
        ((ssh-proc 'serve-ssh) #:port 65536 #:handler echo-handler))
      (test-error "SSH server rejects an empty address" #t
        ((ssh-proc 'serve-ssh) #:address "" #:handler echo-handler))
      (test-error "SSH server rejects malformed authorized keys" #t
        ((ssh-proc 'serve-ssh) #:authorized-keys '(42) #:handler echo-handler))
      (test-error "SSH server enforces the CLI worker limit" #t
        ((ssh-proc 'serve-ssh) #:workers 65 #:handler echo-handler))
      (test-error "SSH server rejects non-positive memory limits" #t
        ((ssh-proc 'serve-ssh) #:memory-limit 0 #:handler echo-handler))
      (test-error "SSH server requires an output port" #t
        ((ssh-proc 'serve-ssh) #:output #f #:handler echo-handler))

      ;; Authorized keys
      (test-equal "authorized keys parse into language-tagged blobs"
        '("AAAAaaa" "BBBBbbb")
        (let ((path (string-append test-directory "/authorized_keys.sample")))
          (call-with-output-file path
            (lambda (port)
              (display "# a comment\n\n" port)
              (display "ssh-rsa AAAAaaa user@host\n" port)
              (display "command=\"x\",no-pty ssh-ed25519 BBBBbbb note\n" port)))
          ((ssh-proc 'read-authorized-keys) path)))

      (test-equal "an empty authorized-keys file yields no keys"
        '()
        (let ((path (string-append test-directory "/authorized_keys.empty")))
          (call-with-output-file path (lambda (port) #t))
          ((ssh-proc 'read-authorized-keys) path)))
      (test-error "authorized keys require a non-empty path" #t
        ((ssh-proc 'read-authorized-keys) ""))

      ;; Host key generation
      (test-error "host key generation requires positive integer bits" #t
        ((ssh-proc 'generate-host-key)
         (string-append test-directory "/invalid-bits") #:bits 0))
      (test-error "host key generation requires a symbol type" #t
        ((ssh-proc 'generate-host-key)
         (string-append test-directory "/invalid-type") #:type "rsa"))
      (test-error "host key force option must be boolean" #t
        ((ssh-proc 'generate-host-key)
         (string-append test-directory "/invalid-force") #:force 'yes))
      (test-error "host key generation requires an output port" #t
        ((ssh-proc 'generate-host-key)
         (string-append test-directory "/invalid-output") #:output #f))
      (test-assert "generate-host-key writes an RSA host key"
        (let ((path (string-append test-directory "/generated_host")))
          ((ssh-proc 'generate-host-key) path #:output (open-output-string))
          (file-exists? path)))

      (test-assert "generate-host-key refuses to overwrite an existing key"
        (let ((path (string-append test-directory "/generated_host")))
          (catch #t
            (lambda () ((ssh-proc 'generate-host-key) path) #f)
            (lambda _ #t))))

      ;; Transport (gated on ssh(1), timeout(1), and the generated keys)
      (if (and keys-ready? (have-program? "ssh") (have-program? "timeout"))
          (let* ((server (start-server echo-handler))
                 (server-pid (car server))
                 (port (cdr server)))
            (dynamic-wind
              (lambda () #t)
              (lambda ()
                (let ((result (ssh-run port client-key "hi\\nq\\n")))
                  (test-equal "an authorized key runs the session handler"
                    0 (car result))
                  (test-assert "the handler receives the client input"
                    (string-contains (cdr result) "echo:hi")))
                (let ((result (ssh-run port other-key "q\\n")))
                  (test-assert "an unauthorized key is rejected"
                    (not (zero? (car result))))))
              (lambda () (stop-server server-pid))))
          (test-assert "SSH transport (skipped: ssh/ssh-keygen unavailable)" #t))))

(test-end "grove ssh")

(for-each (lambda (name)
            (let ((path (string-append test-directory "/" name)))
              (when (file-exists? path) (delete-file path))))
          '("host_rsa" "host_rsa.pub"
            "client_ed25519" "client_ed25519.pub"
            "other_ed25519" "other_ed25519.pub"
            "authorized_keys" "authorized_keys.sample" "authorized_keys.empty"
            "generated_host" "generated_host.pub"))
(rmdir test-directory)
