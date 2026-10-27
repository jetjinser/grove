;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove relation dsl)
  #:use-module (grove relation)
  #:use-module ((grove private minikanren)
                #:select (with-projected-terms))
  #:use-module (srfi srfi-1)
  #:re-export (relation?
               make-relation
               relation-arity
               relation-facts
               relationo
               run
               run*
               defrel
               ==
               =/=
               fresh
               conde
               symbolo
               numbero
               stringo
               absento
               succeed
               fail
               matche)
  #:export (define-relation-schema
            project))

(define (syntax-list-elements form)
  (syntax-case form ()
    (() '())
    ((head . tail)
     (cons #'head (syntax-list-elements #'tail)))
    (_
     (syntax-violation 'define-relation-schema
                       "expected a proper syntax list"
                       form))))

(define (zip-list left right)
  (if (null? left)
      '()
      (cons (cons (car left) (car right))
            (zip-list (cdr left) (cdr right)))))

(define (field-position field fields)
  (let loop ((fields fields) (position 0))
    (cond
      ((null? fields) #f)
      ((eq? field (car fields)) position)
      (else (loop (cdr fields) (+ position 1))))))

(define (unique-field-names? fields)
  (let loop ((fields fields) (seen '()))
    (or (null? fields)
        (and (not (memq (car fields) seen))
             (loop (cdr fields) (cons (car fields) seen))))))

(define (schema-query-terms fields constraints call)
  (let loop ((fields fields) (result '()))
    (if (null? fields)
        (reverse result)
        (let ((constraint (assq (car fields) constraints)))
          (loop (cdr fields)
                (cons (if constraint
                          (cdr constraint)
                          (datum->syntax call '_))
                      result))))))

(define (parse-field-constraints entries fields call)
  (let loop ((entries entries) (constraints '()))
    (if (null? entries)
        constraints
        (let ((field (caar entries))
              (term (cdar entries)))
          (unless (identifier? field)
            (syntax-violation
              #f
              "relation column names must be identifiers"
              call
              field))
          (let ((name (syntax->datum field)))
            (unless (field-position name fields)
              (syntax-violation
                #f
                (format #f "unknown relation column ~a" name)
                call
                field))
            (when (assq name constraints)
              (syntax-violation
                #f
                (format #f "relation column ~a is constrained twice" name)
                call
                field))
            (loop (cdr entries)
                  (cons (cons name term) constraints)))))))

(define (expand-schema-query relation terms call)
  (let* ((temporaries (generate-temporaries
                        (filter (lambda (term)
                                  (eq? (syntax->datum term) '_))
                                terms)))
         (arguments
           (let loop ((terms terms) (temporaries temporaries) (result '()))
             (if (null? terms)
                 (reverse result)
                 (if (eq? (syntax->datum (car terms)) '_)
                     (loop (cdr terms) (cdr temporaries)
                           (cons (car temporaries) result))
                     (loop (cdr terms) temporaries
                           (cons (car terms) result)))))))
    (with-syntax (((temporary ...) temporaries)
                  ((column-value ...) arguments)
                  (relation relation))
      #'(fresh (temporary ...)
          (relationo relation column-value ...)))))

;; Declare a relation's row shape once. The generated constructor fixes its
;; arity, and the generated goal checks call arity while expanding and gives
;; every `_` occurrence its own fresh logic variable.
(define (make-schema-goal-transformer fields)
  (lambda (call)
    (syntax-case call ()
      ((_ relation (column term) ...)
       (let* ((constraints
                (parse-field-constraints
                  (zip-list (syntax-list-elements #'(column ...))
                            (syntax-list-elements #'(term ...)))
                  fields
                  call))
              (terms (schema-query-terms fields constraints call)))
         (expand-schema-query #'relation terms call)))
      (_
       (syntax-violation
         #f
         "expected a relation value followed by (column term) constraints"
         call)))))

(define-syntax define-relation-schema
  (lambda (form)
    (syntax-case form ()
      ((_ constructor goal-name (column ...))
       (let* ((column-syntaxes (syntax-list-elements #'(column ...)))
              (fields (map syntax->datum column-syntaxes))
              (arity (length fields)))
         (unless (and (positive? arity)
                      (every identifier? column-syntaxes)
                      (unique-field-names? fields))
           (syntax-violation
             #f
             "relation schema requires distinct column identifiers"
             form))
         (with-syntax ((arity (datum->syntax #'constructor arity))
                       ((field ...) #'(column ...)))
           #'(begin
               (define (constructor rows)
                 (make-relation rows #:arity arity))
               (define-syntax goal-name
                 (make-schema-goal-transformer '(field ...))))))))))

(define-syntax project
  (syntax-rules ()
    ((_ (term ...) body ...)
     (with-projected-terms (term ...) body ...))))
