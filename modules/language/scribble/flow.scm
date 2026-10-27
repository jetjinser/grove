;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (language scribble flow)
  #:use-module ((grove private ir) #:prefix ir:)
  #:use-module (grove ir)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (decode-flow content-value? content-sequence? flatten-content-sequence))

(define (content-value? value)
  (or (string? value)
      (content-text? value)
      (content-layout? value)
      (content-element? value)
      (content-document? value)))

(define (content-sequence? value)
  (and (list? value)
       (every (lambda (item)
                (or (content-value? item)
                    (content-sequence? item)))
              value)))

(define (flatten-content-sequence values)
  (append-map (lambda (value)
                (if (content-sequence? value)
                    (flatten-content-sequence value)
                    (list value)))
              values))

(define flow-newline (list 'flow-newline))

(define (location-on-next-line location offset)
  (and location
       (let ((line (assq-ref location 'line)))
         (map (lambda (entry)
                (case (car entry)
                  ((line) (cons 'line (+ line offset)))
                  ((column) (cons 'column 1))
                  (else entry)))
              location))))

(define (text-flow-events text source location)
  (let loop ((lines (string-split text #\newline))
             (line-offset 0)
             (events '()))
    (if (null? lines)
        (reverse events)
        (let* ((line (car lines))
               (events (if (string-null? line)
                           events
                           (cons (make-content-text
                                   line source
                                   #:location
                                   (and location
                                        (location-on-next-line
                                          location line-offset)))
                                 events))))
          (loop (cdr lines) (1+ line-offset)
                (if (null? (cdr lines))
                    events
                    (cons flow-newline events)))))))

(define (flow-events value)
  (cond
    ((string? value) (text-flow-events value #f #f))
    ((content-text? value)
     (text-flow-events (ir:content-text-value/raw value)
                       (ir:content-text-source/raw value)
                       (ir:content-text-location/raw value)))
    ((content-layout? value)
     (if (eq? (content-layout-kind value) 'newline)
         (list flow-newline)
         (list value)))
    ((content-document? value)
     (append-map flow-events (ir:content-document-children/raw value)))
    ((content-sequence? value)
     (append-map flow-events (flatten-content-sequence value)))
    (else (list value))))

(define (flow-block? value)
  (and (content-element? value)
       (content-element-block? value)))

(define (flow-whitespace? value)
  (cond
    ((string? value) (string-every char-whitespace? value))
    ((content-text? value)
     (string-every char-whitespace? (ir:content-text-value/raw value)))
    ((content-layout? value)
     (string-every char-whitespace? (ir:content-layout-raw/raw value)))
    (else #f)))

(define (decode-flow values)
  (define (flush output pending)
    (if (or (null? pending) (every flow-whitespace? pending))
        output
        (let* ((contents (reverse pending))
               (contents (drop-while flow-whitespace? contents))
               (contents
                 (reverse (drop-while flow-whitespace? (reverse contents)))))
          (if (null? contents)
              output
              (let* ((paragraph
                       (make-content-element 'paragraph contents
                                             #:flow-kind 'block))
                     (first (car contents))
                     (location
                       (cond
                         ((content-text? first)
                          (ir:content-text-location/raw first))
                         ((content-element? first)
                          (ir:content-element-property/raw first 'source-location))
                         (else #f))))
                (cons (if location
                          (content-with-source-location paragraph location)
                          paragraph)
                      output))))))
  (let loop ((events (append-map flow-events values))
             (pending '())
             (newlines 0)
             (output '()))
    (cond
      ((null? events)
       (reverse (flush output pending)))
      ((eq? (car events) flow-newline)
       (if (= newlines 0)
           (loop (cdr events) pending 1 output)
           (loop (cdr events) '() 0 (flush output pending))))
      ((flow-block? (car events))
       (loop (cdr events) '() 0
             (cons (car events) (flush output pending))))
      ((and (= newlines 1) (flow-whitespace? (car events)))
       (loop (cdr events) pending newlines output))
      (else
       (loop (cdr events)
             (if (and (= newlines 1) (pair? pending))
                 (cons (car events)
                       (cons (make-content-layout 'newline "\n") pending))
                 (cons (car events) pending))
             0
             output)))))

