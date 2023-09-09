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




;Retorna una lista de tuplas que representen el producto cartesiano entre L1 y L2. 
;cartesian-product : List -> List
;usage: (list-length l) = Lista del producto cartesiano entre L1 Y L2
(define cartesian-product
  (lambda (L1 L2)
    (if (null? L1)
        '()
        (if (null? L2)
            '()
            (cons (list (car L1) (car L2))
                  (append (cartesian-product (list (car L1)) (cdr L2))
                          (cartesian-product (cdr L1) L2)))))))