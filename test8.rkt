#lang racket

(struct Closure (f env))

(match (Closure '(lambda (x) (* 2 x)) '(x . 3) )

       [(Closure `(lambda (,x) ,e) env-save)
        ((lambda ()
           (print env-save) ; x=>'x , e => '(* 2 x), env-save => '(x . 3)
           ))
        ]
       )

