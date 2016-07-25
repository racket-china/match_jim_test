#lang racket
(struct Closure (f env))

;;(let ( [ a (Closure '(lambda (x) (* y x)) '((y . 3))) ]
;;       )
;;  (print a))
;;(match 

;;(let ( [aa
(match
 '(lambda (x) (+ x y))
 [`(lambda (,x) ,e)
  ((lambda ()
     (print "----------")
     (print e) ; e => '(+ x y) , x => 'x
     (Closure x e)
     )) 
  ]) ;; => #<Closure>

;;            ] )
;;  ;;;
;;  (match aa
;;         [`(Closure ,x) 1] 
;;         )
;;  )
;; [(Closure `(lambda (,x) ,e))
;;  ]
;; )


(match
 '((lambda (x) (+ x 3) ) 2)
 [`(,x ,e)
  ((lambda ()
     (print "+++++++++")
     (print x) ; x => '(lambda (x) (+ x 3)) , e => 2
     ;;(Closure x e)
     )) 
  ]) ;; 
