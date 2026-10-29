;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (ice-9 match)
             (ice-9 rdelim)
             (srfi srfi-1)
             (srfi srfi-9)
             (grove project)
             (grove output))

(define-record-type <product>
  (make-product sku name category quantity)
  product?
  (sku product-sku)
  (name product-name)
  (category product-category)
  (quantity product-quantity))

(define-record-type <inventory>
  (make-inventory categories products)
  inventory?
  (categories inventory-categories)
  (products inventory-products))

(define example-directory (dirname (current-filename)))
(define data-directory (string-append example-directory "/data"))
(define products-path (string-append data-directory "/products.csv"))
(define categories-path (string-append data-directory "/categories.txt"))

(define (read-product-file path)
  (let ((lines (call-with-input-file path
                 (lambda (port)
                   (let loop ((lines '()) (line (read-line port)))
                     (if (eof-object? line)
                         (reverse lines)
                         (loop (cons line lines) (read-line port))))))))
    (map (lambda (line)
           (match (string-split line #\,)
             ((sku name category quantity)
              (make-product sku name category (string->number quantity)))))
         (cdr lines))))

(define (category-order)
  (filter-map
    (lambda (line)
      (let ((category (string-trim-both line)))
        (and (not (string-null? category))
             (not (string-prefix? ";;" category))
             category)))
    (call-with-input-file categories-path
      (lambda (port)
        (let loop ((lines '()) (line (read-line port)))
          (if (eof-object? line)
              (reverse lines)
              (loop (cons line lines) (read-line port))))))))

(define (analyze-inventory context)
  (let* ((categories (category-order))
         (products
           (append-map project-input-value
                       (project-context-inputs context))))
    (for-each
      (lambda (product)
        (unless (member (product-category product) categories string=?)
          (error "product has an unknown category" product)))
      products)
    (make-inventory categories products)))

(define (inventory-source-paths inventory)
  (list products-path categories-path))

(define (category-count inventory category)
  (fold (lambda (product count)
          (if (string=? category (product-category product))
              (+ count (product-quantity product))
              count))
        0
        (inventory-products inventory)))

(define (render-summary-csv inventory)
  (list
    (make-artifact
      "summary.csv"
      (call-with-output-string
        (lambda (port)
          (display "category,total_quantity\n" port)
          (for-each
            (lambda (category)
              (format port "~a,~a\n" category
                      (category-count inventory category)))
            (inventory-categories inventory))))
      #:sources (inventory-source-paths inventory))))

(define (render-scheme-data inventory)
  (list
    (make-artifact
      "inventory.scm"
      (call-with-output-string
        (lambda (port)
          (write
            (list 'inventory
                  (map (lambda (product)
                         (list (product-sku product)
                               (product-name product)
                               (product-category product)
                               (product-quantity product)))
                       (inventory-products inventory)))
            port)
          (newline port)))
      #:sources (inventory-source-paths inventory))))

(define project
  (make-project
    #:collect-sources (lambda () (list products-path))
    #:prepare (lambda () (list categories-path))
    #:read-source read-product-file
    #:analyze analyze-inventory
    #:renderers (list render-summary-csv render-scheme-data)))

project
