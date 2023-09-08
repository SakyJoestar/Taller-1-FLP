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

;Retorna una lista que contiene los elementos que pertenecen a L y que satisfacen el predicado P
;filter-in : List -> List
;usage: (list-length l) = Lista de los elementos que cumplen P
(define filter-in
  (lambda (P L)
    (if (null? L)
        '()
        (if (P (car L))
            (cons (car L) (filter-in P (cdr L)))
            (filter-in P (cdr L))))))