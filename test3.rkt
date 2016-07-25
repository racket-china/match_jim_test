#lang racket

(define ext-env
  (lambda (x v env)
    (cons `(,x . ,v) env)))

(define lookup
  (lambda (x env)
    (let ([p (assq x env)])
      (cond
       [(not p) #f]
       [else (cdr p)]))))

(print

 (ext-env 'a 1 '()) ;; => '((a . 1))

 (lookup 'a '((a . 1))) ;;=> 1
 
 )
