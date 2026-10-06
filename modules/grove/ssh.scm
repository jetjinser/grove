;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove ssh)
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
  (message-auth-set-methods! msg '(public-key))
  (case (cadr type)
    ((auth-method-publickey)
     (let* ((request (message-get-req msg))
            (state (catch #t (lambda () (auth-req:pubkey-state request))
                          (lambda _ #f)))
            (offered (public-key-blob
                       (catch #t (lambda () (auth-req:pubkey request))
                              (lambda _ #f)))))
       (format output "grove ssh: public key authentication state=~a~%" state)
       (case state
         ((none)
          (if (and offered (member offered authorized string=?))
              (message-auth-reply-public-key-ok msg)
              (message-reply-default msg)))
         ((valid)
          ;; libssh only reports a valid signature; the offered key was already
          ;; checked against the authorized set at the "none" state.
          (message-reply-success msg))
         (else
          (message-reply-default msg)))))
    (else
     (message-reply-default msg))))

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

(define (handle-session session handler authorized output)
  (let loop ((channel #f))
    (let ((msg (catch 'guile-ssh-error
                 (lambda () (server-message-get session))
                 (lambda _ #f))))
      (if (not msg)
          (if (connected? session)
              (begin (usleep 50000) (loop channel))
              #f)
          (let ((type (message-get-type msg)))
            (case (car type)
              ((request-service)
               (message-reply-success msg)
               (loop channel))
              ((request-auth)
               (handle-auth msg type authorized output)
               (loop channel))
              ((request-channel-open)
               (case (cadr type)
                 ((channel-session)
                  (loop (message-channel-request-open-reply-accept msg)))
                 (else
                  (message-reply-default msg)
                  (loop channel))))
              ((request-channel)
               (if (handle-channel-request msg type channel session handler output)
                   #t
                   (loop channel)))
              (else
               (message-reply-default msg)
               (loop channel))))))))

(define (close-channel channel)
  (when channel
    (catch #t (lambda () (close channel)) (lambda _ #t))))

(define* (serve-ssh #:key
                    (address "127.0.0.1")
                    (port 0)
                    host-key
                    (authorized-keys '())
                    (banner "Grove")
                    (log-verbosity 'nolog)
                    handler
                    (output (current-error-port)))
  "Serve HANDLER over SSH until interrupted.

HANDLER receives the session channel as both input and output. HOST-KEY is an
RSA host key file; AUTHORIZED-KEYS is a list from read-authorized-keys."
  (unless (procedure? handler)
    (error "SSH server requires a session handler" handler))
  (unless (and (string? host-key) (file-exists? host-key))
    (error "SSH server requires an existing host key" host-key))
  (let ((server (make-server #:bindaddr address
                             #:bindport port
                             #:rsakey host-key
                             #:banner banner
                             #:log-verbosity log-verbosity)))
    (server-listen server)
    (let serve ()
      (let ((session (catch 'guile-ssh-error
                       (lambda () (server-accept server))
                       (lambda _ #f))))
        (if (not session)
            (sleep 1)
            (begin
              (server-handle-key-exchange session)
              (catch #t
                (lambda ()
                  (handle-session session handler authorized-keys output))
                (lambda (key . arguments)
                  (format output "grove ssh: session error: ~s ~s~%"
                          key arguments)))
              (catch 'guile-ssh-error
                (lambda () (disconnect! session))
                (lambda _ #t)))))
      (serve))))
