;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove relation)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (grove relation))

(test-begin "grove relation")

(test-assert "relation indexes are not part of the public interface"
  (not (module-variable
         (module-public-interface (resolve-module '(grove relation)))
         'relation-indexes)))

(define links
  (make-relation
    '((alpha beta)
      (alpha gamma)
      (beta delta)
      (gamma delta)
      (delta alpha)
      (alpha beta))))

(test-assert "relation constructor validates its facts"
  (relation? links))

(test-equal "relation records its arity"
  2
  (relation-arity links))

(test-equal "relations have set semantics for duplicate facts"
  5
  (length (relation-facts links)))

(test-equal "fact deduplication preserves first occurrence order"
  '((alpha beta)
    (alpha gamma)
    (beta delta)
    (gamma delta)
    (delta alpha))
  (relation-facts links))

(test-error "relation arity must be an exact positive integer" #t
  (make-relation '() #:arity 2.0))

(test-error "relation arity must be positive" #t
  (make-relation '() #:arity 0))

(let* ((source (string-copy "source"))
       (target (string-copy "target"))
       (row (list source (list 'tag target)))
       (relation (make-relation (list row))))
  (string-set! source 0 #\x)
  (string-set! target 0 #\x)
  (set-car! row "changed")
  (test-equal "relations snapshot caller-owned strings and pair structure"
    '(("source" (tag "target")))
    (relation-facts relation))
  (let ((facts (relation-facts relation)))
    (set-car! facts '(changed . #t)))
  (let ((facts (relation-facts relation)))
    (string-set! (cadr (cadar facts)) 0 #\x))
  (test-equal "relation-facts returns an independent value snapshot"
    '(("source" (tag "target")))
    (relation-facts relation))
  (let ((answer
         (car (run* (value)
                (relationo relation "source" value)))))
    (string-set! (cadr answer) 0 #\x))
  (test-equal "query answers cannot mutate relation facts or indexes"
    '(("source" (tag "target")))
    (relation-facts relation))
  (test-equal "queries remain valid after callers mutate their snapshots"
    '((tag "target"))
    (run* (value)
      (relationo relation "source" value))))

(let* ((bytes (u8-list->bytevector '(1 2 3)))
       (relation (make-relation (list (list bytes)) #:arity 1)))
  (bytevector-u8-set! bytes 0 9)
  (test-equal "relations snapshot mutable bytevector terms"
    '(1 2 3)
    (bytevector->u8-list (car (car (relation-facts relation)))))
  (let ((facts (relation-facts relation)))
    (bytevector-u8-set! (car (car facts)) 1 9))
  (test-equal "relation facts copy bytevector terms on access"
    '(1 2 3)
    (bytevector->u8-list (car (car (relation-facts relation)))))
  (let ((answer (car (run* (value) (relationo relation value)))))
    (bytevector-u8-set! answer 2 9))
  (test-equal "relation query answers cannot mutate bytevector facts"
    '(1 2 3)
    (bytevector->u8-list (car (car (relation-facts relation))))))

(test-equal "fact deduplication compares compound terms structurally"
  2
  (length
    (relation-facts
      (make-relation
        (list (list '(tag alpha) 'topic)
              (list (list 'tag 'alpha) 'topic)
              (list '(tag beta) 'topic))))))

(test-equal "a bound source uses the outgoing relation"
  '(beta gamma)
  (run* (target)
    (relationo links 'alpha target)))

(test-equal "a bound target uses the incoming relation"
  '(alpha)
  (run* (source)
    (relationo links source 'beta)))

(test-equal "two bound terms check relation membership"
  '(present)
  (run* (answer)
    (relationo links 'alpha 'beta)
    (== answer 'present)))

(test-equal "two bound terms reject missing facts"
  '()
  (run* (answer)
    (relationo links 'beta 'alpha)
    (== answer 'present)))

(test-equal "unbound terms enumerate all unique facts"
  (relation-facts links)
  (run* (fact)
    (fresh (source target)
      (== fact (list source target))
      (relationo links source target))))

(test-error "relation goal rejects an arity mismatch"
  #t
  (run* (value)
    (relationo links value)))

(let ((references
        (make-relation
          '((page-a page-b #f)
            (page-a page-c introduction)
            (page-b page-c #f))
          #:arity 3)))
  (test-equal "n-ary relations record their arity and join columns"
    '((page-a introduction) (page-b #f))
    (run* (source fragment)
      (relationo references source 'page-c fragment)))
  (test-equal "n-ary relation goals can bind a later column"
    '((page-a page-c))
    (run* (source target)
      (relationo references source target 'introduction)))
  (test-equal "n-ary relations enumerate their rows"
    (relation-facts references)
    (run* (source target fragment)
      (relationo references source target fragment))))

(test-equal "empty relations require and retain an explicit non-default arity"
  '(1 ())
  (let ((empty (make-relation '() #:arity 1)))
    (list (relation-arity empty)
          (run* (value)
            (relationo empty value)))))

(test-error "relation constructor rejects rows with inconsistent arity"
  #t
  (make-relation '((first second) (only-first))))

(test-equal "relation goals compose through a shared logic variable"
  '(beta gamma)
  (run* (middle)
    (fresh (target)
      (relationo links 'alpha middle)
      (relationo links middle target)
      (== target 'delta))))

(let ((references
        (make-relation
          '((first left)
            (first right)
            (second left)
            (second right)
            (third left)
            (unrelated elsewhere)))))
  (test-equal "a shared logic variable joins two indexed relation lookups"
    '(first second)
    (run* (source)
      (relationo references source 'left)
      (relationo references source 'right))))

(let ((structured
        (make-relation
          (list (list '(tag alice) 'topic)
                (list '(tag bob) 'topic)))))
  (test-equal "an indexed lookup unifies structured terms with variables"
    '(alice bob)
    (run* (name)
      (relationo structured (list 'tag name) 'topic))))

(define (not-membero item items)
  (conde
    ((== '() items))
    ((fresh (head tail)
       (== `(,head . ,tail) items)
       (=/= item head)
       (not-membero item tail)))))

(define (reachableo relation source target)
  (walko relation source target (list source)))

(define (walko relation source target seen)
  (conde
    ((relationo relation source target)
     (not-membero target seen))
    ((fresh (middle)
       (relationo relation source middle)
       (not-membero middle seen)
       (walko relation middle target (cons middle seen))))))

(test-equal "recursive relation queries terminate on cycles"
  '(beta delta gamma)
  (sort
    (delete-duplicates
      (run* (target)
        (reachableo links 'alpha target))
      equal?)
    (lambda (left right)
      (string<? (symbol->string left) (symbol->string right)))))

(test-error "relation constructor rejects non-row facts"
  #t
  (make-relation '((alpha beta) broken)))

(test-error "relation constructor rejects vector terms reserved for logic variables"
  #t
  (make-relation (list (list 'alpha (vector 'tag 'value)))))

(test-error "relation constructor rejects vectors nested in structured terms"
  #t
  (make-relation (list (list 'alpha (list 'metadata (vector 'tag 'value))))))

(let ((cycle (cons 'loop '())))
  (set-cdr! cycle cycle)
  (test-error "relation constructor rejects cyclic terms"
    #t
    (make-relation (list (list 'alpha cycle)))))

(test-error "relationo rejects non-relations"
  #t
  (run* (value)
    (relationo 'not-a-relation 'alpha 'beta)))

(test-end "grove relation")
