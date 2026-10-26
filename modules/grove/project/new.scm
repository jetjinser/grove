;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove project new)
  #:use-module (grove project)
  #:export (create-source))

(define* (create-source project #:key prefix destination)
  "Create a project source through its configured source creator.

PREFIX and DESTINATION are passed to the project-owned creation procedure.
Projects without such a procedure cannot use `grove new`."
  (let ((creator (project-create-source project)))
    (unless creator
      (error "this project does not define how to create a source file"))
    (creator prefix destination)))
