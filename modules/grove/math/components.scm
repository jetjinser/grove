;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later
(define-module (grove math components)
  #:use-module (grove relation)
  #:use-module (grove math rule)
  #:use-module (grove math types)
  #:export (math-component-rules))

(define math-component-rules
  (list
    (math-rule 'components-sum "components-sum" "Cartesian scalar entries" 'components
      (facts a b c d e f) (+ (components ,a ,b ,c) (components ,d ,e ,f)) (components (+ ,a ,d) (+ ,b ,e) (+ ,c ,f))
      () ())
    (math-rule 'components-difference "components-difference" "Cartesian scalar entries" 'components
      (facts a b c d e f) (- (components ,a ,b ,c) (components ,d ,e ,f)) (components (- ,a ,d) (- ,b ,e) (- ,c ,f))
      () ())
    (math-rule 'components-negation "components-negation" "Cartesian scalar entries" 'components
      (facts a b c) (- (components ,a ,b ,c)) (components (- ,a) (- ,b) (- ,c))
      () ())
    (math-rule 'components-scale-left "components-scale-left" "Cartesian scalar entries" 'components
      (facts s a b c) (* ,s (components ,a ,b ,c)) (components (* ,s ,a) (* ,s ,b) (* ,s ,c))
      () ((math-typeo facts s 'scalar)))
    (math-rule 'components-scale-right "components-scale-right" "Cartesian scalar entries" 'components
      (facts s a b c) (* (components ,a ,b ,c) ,s) (components (* ,s ,a) (* ,s ,b) (* ,s ,c))
      () ((math-typeo facts s 'scalar)))
    (math-rule 'components-quotient "components-quotient" "Cartesian scalar entries" 'components
      (facts s a b c) (/ (components ,a ,b ,c) ,s) (components (/ ,a ,s) (/ ,b ,s) (/ ,c ,s))
      () ((math-typeo facts s 'scalar)))
    (math-rule 'components-dot "components-dot" "Cartesian scalar entries" 'components
      (facts a b c d e f) (dot (components ,a ,b ,c) (components ,d ,e ,f)) (+ (+ (* ,a ,d) (* ,b ,e)) (* ,c ,f))
      () ())
    (math-rule 'components-cross "components-cross" "Cartesian scalar entries" 'components
      (facts a b c d e f) (cross (components ,a ,b ,c) (components ,d ,e ,f)) (components (- (* ,b ,f) (* ,c ,e)) (- (* ,c ,d) (* ,a ,f)) (- (* ,a ,e) (* ,b ,d)))
      () ())
    (math-rule 'components-zero "components-zero" "Cartesian scalar entries" 'components
      (facts ) (zero vector) (components (num "0") (num "0") (num "0"))
      () ())))
