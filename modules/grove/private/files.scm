;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private files)
  #:export (file-signature
            regular-file?
            ensure-directory
            call-with-staging-file!))

(define (file-signature path)
  "Identify PATH's target file, including same-size edits and replacements.
Return #f when PATH is missing or cannot be inspected."
  (let ((info (false-if-exception (stat path))))
    (and info
         (list (stat:type info) (stat:dev info) (stat:ino info) (stat:size info)
               (stat:mtime info) (stat:mtimensec info)
               (stat:ctime info) (stat:ctimensec info)))))

(define* (regular-file? path #:key (follow-links? #t))
  "Return #t when PATH resolves to a regular file.

When FOLLOW-LINKS? is false, inspect PATH itself instead of its target."
  (let ((info (false-if-exception (stat path follow-links?))))
    (and info (eq? (stat:type info) 'regular))))

(define (ensure-directory directory)
  "Create DIRECTORY and its missing parents, like mkdir -p.  Tolerant
of another process creating the same directory concurrently."
  (unless (or (string=? directory ".") (file-exists? directory))
    (let ((parent (dirname directory)))
      (unless (string=? parent directory)
        (ensure-directory parent))
      (false-if-exception (mkdir directory))))
  #t)

(define (call-with-staging-file! path write!)
  "Install the bytes WRITE! produces over PATH atomically.  The
staging file reserves a unique mkstemp! name, so concurrent writers
of one path never share a temp file and every rename installs
complete bytes; the port serializes strings as UTF-8 regardless of
the ambient locale, and the file lands with the umask-derived mode
a plain write would have produced.  A failure leaves PATH untouched
and removes the staging file."
  (let ((temp (string-append path ".tmp-XXXXXX")))
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (let ((port (mkstemp! temp)))
          (dynamic-wind
            (lambda () #t)
            (lambda ()
              (set-port-encoding! port "UTF-8")
              (write! port)
              (force-output port))
            (lambda () (close-port port))))
        (chmod temp (logand #o666 (lognot (umask))))
        (rename-file temp path))
      (lambda ()
        (false-if-exception (delete-file temp))))))
