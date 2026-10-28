;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private module-load)
  #:use-module (grove private procedure)
  #:export (call-with-module-load-observer))

(define (call-with-module-load-observer observe thunk)
  "Call THUNK while OBSERVE sees each Guile module load.
Existing load observers remain in the chain, and the previous hook is restored
when THUNK exits or raises."
  (unless (procedure-accepts-arguments? observe 1)
    (error "module load observer must accept a module path" observe))
  (unless (procedure-accepts-arguments? thunk 0)
    (error "module load observation requires a zero-argument thunk" thunk))
  (let ((previous %load-hook))
    (dynamic-wind
      (lambda ()
        (set! %load-hook
          (lambda (file)
            (when previous (previous file))
            (observe file))))
      thunk
      (lambda () (set! %load-hook previous)))))
