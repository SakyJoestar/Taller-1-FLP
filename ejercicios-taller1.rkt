;; Nombre
;; Samuel Galindo 2177491
;; Nombre

#lang eopl

;; list-set :
;; Proposito:
;; L x N x X -> 'L: retorna una lista similar a la que recibe (L), pero
;; debe tener en la posicion ingresada n (indexando desde cero) el elemento x.

(define list-set 
  (lambda(L n x)
    (if (zero? n)
      (cons x (cdr L))
      (cons (car L) (list-set (cdr L) (- n 1) x))
      )
    )
  )
   
;; Pruebas
(list-set '(a b c d) 2 '(1 2))
(list-set '(a b c d) 3 '(1 5 10))
    
;; swapper :
;; Proposito:
;; E1 x E2 x L -> 'L: retorna una lista similar a L, solo que cada ocurrencia
;; anterior de E1 sera reemplazada por E2y cada ocurrencia anterior de E2
;; sera reemplazada por E1 (Los elementos E1 y E2 deben pertenecer a L).
;; retorna una listasimilar a L, solo que cada ocurrencia anterior de E1
;; sera reemplazada por E2 y cada ocurrencia anterior de E2 sera reemplazada
;; por E1 (Los elementos E1 y E2 deben pertenecer a L).


(define swapper 
  (lambda(E1 E2 L)
    (cond
      [(null? L) '()]
      [(eqv?  E1 (car L)) (cons E2 (swapper E1 E2 (cdr L)))]
      [(eqv?  E2 (car L)) (cons E1 (swapper E1 E2 (cdr L)))]
      [else (cons(car L) (swapper E1 E2 (cdr L)))]
      )
    )
  )

;; Pruebas
(swapper 'a 'd '(a b c d))
(swapper 'a 'd '(a d () c d))
(swapper 'x 'y '(y y x y x y x x y))


;; inversions :
;; Proposito:
;; L -> x: determina el numero de inversiones de la lista L. De manera formal,
;; sea A = (a1a2:::an) una lista de n numeros diferentes, si i < j (posicion)
;; y ai > aj (dato en la posicion) entonces la pareja (i j) es una inversion de A.

(define inversions
  (lambda (L)
    (if (null? L)
     0
    (letrec
        (
         (compare
          (lambda (n l)
            (cond
              [(null? l) 0]
              [(> n (car l)) (+ 1 (compare n (cdr l)))]
              [ else (compare n (cdr l))]
              )
            )
          )
         )
      (+ (compare (car L) (cdr L)) (inversions (cdr L)))
      )
    )
    )
  )

;; Pruebas
(inversions '(2 3 8 6 1))
(inversions '(1 2 3 4))
(inversions '(3 2 1))


;; filter-acum :
;; Proposito: retornar la operac
;; a x b x F x acum x filter -> x: El procedimiento filter-acum aplicara la
;; funcion binaria F a todos los elementos que estan en el intervalo [a; b] y que
;; a su vez todos estos elementos cumplen con el predicado de la funcion filter,
;; el resultado se debe ir conservando en acum y debe retornarse el valor final
;; de acum.

(define filter-acum
  (lambda (a b F acum filter)
    (if (<= a b)
        (if (filter a)
            (filter-acum (+ a 1) b F (F a acum) filter)
            (filter-acum (+ a 1) b F acum filter)
            )
        acum
        )
    )
  )

;; Pruebas
(filter-acum 1 10 + 0 odd?)
(filter-acum 1 10 + 0 even?)


  
            