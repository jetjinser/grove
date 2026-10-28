;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private module-load)
  #:export (call-with-module-load-observer))

(define (call-with-module-load-observer observe thunk)
  "Call THUNK while OBSERVE sees each Guile module load.
Existing load observers remain in the chain, and the previous hook is restored
when THUNK exits or raises."
  (unless (procedure? observe)
    (error "module load observer must be a procedure" observe))
  (unless (procedure? thunk)
    (error "module load observation requires a thunk" thunk))
  (let ((previous %load-hook))
    (dynamic-wind
      (lambda ()
        (set! %load-hook
          (lambda (file)
            (when previous (previous file))
            (observe file))))
      thunk
      (lambda () (set! %load-hook previous)))))
