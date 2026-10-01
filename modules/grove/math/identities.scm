;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math identities)
  #:use-module (grove relation)
  #:use-module (grove math rule)
  #:use-module (grove math types)
  #:export (math-identity-rules))

(define (zeroo type term)
  (conde ((== type 'scalar) (== term '(num "0")))
         ((== type 'vector) (== term '(zero vector)))))

(define math-identity-rules
  (list
    (math-rule 'subtract-zero "0 - X = -X" "matching scalar or vector types" 'identity
      (facts term zero type) (- ,zero ,term) (- ,term) ()
      ((math-typeo facts term type) (zeroo type zero))
      #:backward #t #:backward-default? #t)
    (math-rule 'add-zero "X + 0 = X" "matching scalar or vector types" 'identity
      (facts term zero type) (+ ,term ,zero) ,term ()
      ((math-typeo facts term type) (zeroo type zero)) #:backward #t)
    (math-rule 'zero-add "0 + X = X" "matching scalar or vector types" 'identity
      (facts term zero type) (+ ,zero ,term) ,term ()
      ((math-typeo facts term type) (zeroo type zero)) #:backward #t)
    (math-rule 'subtract-right-zero "X - 0 = X" "matching scalar or vector types" 'identity
      (facts term zero type) (- ,term ,zero) ,term ()
      ((math-typeo facts term type) (zeroo type zero)) #:backward #t)
    (math-rule 'double-negation "-(-X) = X" "X is scalar or vector" 'identity
      (facts term type) (- (- ,term)) ,term ()
      ((math-typeo facts term type) (conde ((== type 'scalar)) ((== type 'vector))))
      #:backward #t)
    (math-rule 'multiply-one "X * 1 = X" "X is scalar or vector" 'identity
      (facts term type) (* ,term (num "1")) ,term ()
      ((math-typeo facts term type) (conde ((== type 'scalar)) ((== type 'vector))))
      #:backward #t)
    (math-rule 'one-multiply "1 * X = X" "X is scalar or vector" 'identity
      (facts term type) (* (num "1") ,term) ,term ()
      ((math-typeo facts term type) (conde ((== type 'scalar)) ((== type 'vector))))
      #:backward #t)
    (math-rule 'multiply-zero "X * 0 = 0" "X is scalar" 'identity
      (facts term) (* ,term (num "0")) (num "0") () ((math-typeo facts term 'scalar)))
    (math-rule 'zero-multiply "0 * X = 0" "X is scalar" 'identity
      (facts term) (* (num "0") ,term) (num "0") () ((math-typeo facts term 'scalar)))
    (math-rule 'power-one "X ^ 1 = X" "X is scalar" 'identity
      (facts term) (^ ,term (num "1")) ,term () ((math-typeo facts term 'scalar)))
    (math-rule 'power-zero "X ^ 0 = 1" "polynomial power convention" 'identity
      (facts term) (^ ,term (num "0")) (num "1") () ((math-typeo facts term 'scalar)))))
