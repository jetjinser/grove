;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove relation)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (grove relation))

(test-begin "grove relation")

(define links
  (make-relation
    '((alpha . beta)
      (alpha . gamma)
      (beta . delta)
      (gamma . delta)
      (delta . alpha)
      (alpha . beta))))

(test-assert "relation constructor validates its facts"
  (relation? links))

(test-equal "relations have set semantics for duplicate facts"
  5
  (length (relation-facts links)))

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
      (== fact (cons source target))
      (relationo links source target))))

(test-equal "relation goals compose through a shared logic variable"
  '(beta gamma)
  (run* (middle)
    (fresh (target)
      (relationo links 'alpha middle)
      (relationo links middle target)
      (== target 'delta))))

(let ((structured
        (make-relation
          (list (cons '(tag alice) 'topic)
                (cons '(tag bob) 'topic)))))
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

(test-error "relation constructor rejects non-pair facts"
  #t
  (make-relation '((alpha beta) broken)))

(test-error "relationo rejects non-relations"
  #t
  (relationo 'not-a-relation 'alpha 'beta))

(test-end "grove relation")
