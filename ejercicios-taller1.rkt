;; Nombre
;; Samuel Galindo 2177491
;; Nombre
;; Nicolas Herrera 2182551

#lang eopl

;Invierte los elementos de los pares dentro de una lista
;invert : List -> List
;usage: (list-length l) = Invierte los pares de L
(define invert
  (lambda(L)
    (if (null? L)
    '()
    (cons
     (list (cadr(car L)) (car(car L)))
     (invert(cdr L))))))