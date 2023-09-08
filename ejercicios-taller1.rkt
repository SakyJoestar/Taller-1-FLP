;; Nombre
;; Samuel Galindo 2177491
;; Nombre

#lang eopl

;; list-set :
;; Proposito:
;; L x N x X -> 'L: Reemplaza el elemento de la posicion N (N >= 0)
;; de L por el elemento X.
(define list-set 
  (lambda(lista n x)
    (if (zero? n)
      (cons x (cdr lista))
      (cons (car lista) (list-set (cdr lista) (- n 1) x))
      )
    )
  )
   

;; prueba
(list-set '(a b c d) 2 '(1 2))
(list-set '(a b c d) 3 '(1 5 10))
    
