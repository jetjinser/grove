;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private style i18n)
  #:use-module (grove output)
  #:use-module (grove private string-fun)
  #:use-module (grove style)
  #:use-module (grove style html)
  #:use-module (ice-9 optargs)
  #:export (bilingual-container-style
            bilingual-fold-container-style
            bilingual-pair-style
            bilingual-source-style
            bilingual-lane-style
            foreign-term-style
            default-bilingual-style))

;; Bilingual content is lowered to ordinary content elements whose HTML
;; presentation is expressed entirely through the existing style API. That
;; keeps the default HTML renderer usable without any renderer-specific case.
;;
;; A user style supplies the class or extra attributes; the structural tag and
;; language attributes are appended so presentation never changes an element's
;; flow kind (for example, a lane must stay a block).

(define (with-html-presentation style properties)
  "Extend STYLE with structural HTML PROPERTIES, keeping its class and extras."
  (cond
    ((not style) (make-content-style #f properties))
    ((content-style? style)
     (make-content-style (content-style-name style)
       (append (content-style-properties style) properties)))
    ((or (symbol? style) (string? style))
     (make-content-style style properties))
    (else (error "content style must be a style name, content style, or #f"
                 style))))

(define* (bilingual-container-style #:optional (style #f))
  (with-html-presentation style
    (list (make-html-tag 'div)
          (make-html-attributes '((class . "bilingual"))))))

(define* (bilingual-fold-container-style #:optional (style #f))
  "A bilingual container whose reader language is shown and whose sources fold."
  (with-html-presentation style
    (list (make-html-tag 'div)
          (make-html-attributes
            '((class . "bilingual") (data-mode . "fold"))))))

(define bilingual-pair-style
  (make-content-style #f
    (list (make-html-tag 'div)
          (make-html-attributes '((class . "bilingual-pair"))))))

;; A folded source is a <details>, so its style only adds the class and leaves
;; the element's tag alone.

(define bilingual-source-style
  (make-content-style 'bilingual-source))

(define* (bilingual-lane-style tag dir #:optional (style #f))
  "Present one language lane as a block with an explicit language and direction."
  (unless (string? tag)
    (error "bilingual lane tag must be a string" tag))
  (with-html-presentation style
    (list (make-html-tag 'div)
          (make-html-attributes
            `((class . "bilingual-lane") (lang . ,tag) (dir . ,dir))))))

(define* (foreign-term-style tag dir #:optional (style #f))
  "Mark inline foreign-language text without changing its flow."
  (unless (string? tag)
    (error "foreign term tag must be a string" tag))
  (with-html-presentation style
    (list (make-html-attributes `((lang . ,tag) (dir . ,dir))))))

(define bilingual-stylesheet
  (make-artifact
    "i18n.css"
    (include-text "forester/style/i18n.css")
    (list "modules/forester/private/style/i18n.scm"
          "modules/forester/style/i18n.css")))

(define default-bilingual-style
  (make-html-style #:stylesheets (list bilingual-stylesheet)))
