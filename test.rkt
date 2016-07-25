#lang racket

(define tree-sum
  (lambda (exp)
    (match exp
           [(? number? x) x]
           [`(,e1 ,e2)
            (let ([v1 (tree-sum e1)]
                  [v2 (tree-sum e2)])
              (+ v1 v2))])))

(print
 (tree-sum '((1 2) (3 4))) ;;=> 10
 (let ((a 2)) `(+ ,a 5) ) ;; => '(+ 2 5)
 (number? 1) ;=> #t
 (match 123 [(? number? x) (+ 100 x)]) ;;=> 223
 
 (match '(1 2) [`(,e1 ,e2) 222 ] ) ;; => 222

 (match '(1 2) [`(,e1 ,e2) (list "==" e1 "==" e2) ] ) ;;=> '("==" 1 "==" 2)
 
 )
