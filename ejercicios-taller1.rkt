#|
Fundamentos de Interpretación y Compilación de Lenguajes de Programación
750017C - G01

Taller 1: Definición recursiva de programas e inducción

Autores:
Samuel Galindo 2177491
Nicolás Herrera 2182551
Christian Vargas 2179172
|#


#lang eopl



;; ********************* Funciones propias *********************
;; mi-append
;; Proposito:
;; L1 x L2 -> L' : Retorna una lista que contiene los elementos que pertenecen a L1 y L2.
;;
;; <lista> := () | (<SchemeValue> <lista>)

(define mi-append
  (lambda (L1 L2)
    (if (null? L1)
      L2
      (cons (car L1) (mi-append (cdr L1) L2)))))

;; Pruebas
(mi-append '(E d 3 "3fe" 1) '(4 n ?))
(mi-append '() '(4 n ?))
(mi-append '(22 45 5) '())
(mi-append '(22 45 5) '(5 34 63 3))



;; ********************* Problema 1 *********************
;; invert:
;; Propósito:
;; L -> L' : Invierte los pares de L.
;;
;; <lista> := () | (<par> <lista>)
;; <par>   := (<SchemeValue> <SchemeValue>)

(define invert
  (lambda(L)
    (if (null? L)
        '()
        (cons
         (list (cadr(car L)) (car(car L)))
         (invert(cdr L))))))

;; Pruebas
(invert '((a 1) (a 2) (1 b) (2 b)))
(invert '((5 9) (10 91) (82 7) (a e) ("hola" "Mundo")))
(invert '(("es" "racket") ("genial" "muy") (17 29) (81 o)))



;; ********************* Problema 2 *********************
;; down:
;; Propósito:
;; L -> L' : Retorna una lista con cada elemento de L asociado a un
;;           nivel de paréntesis mayor comparado con su estado original.
;; <lista> := ( {<valor-de-scheme>}* )

(define down
  (lambda (list-of-values)
    (if (null? list-of-values)
        '()
        (cons
         (list (car list-of-values))
         (down (cdr list-of-values))))))

;; Pruebas
(down '(1 2 3))
(down '((una) (buena) (idea)))
(down '(un (objeto (mas)) complicado))



;; ********************* Problema 4 *********************
;; filter-in:
;; Propósito:
;; P x L -> L' : Retorna una lista que contiene los elementos
;;               que pertenecen a L y que satisfacen el predicado P.
;; <lista> := () | (<SchemeValue> <lista>)

(define filter-in
  (lambda (P L)
    (if (null? L)
        '()
        (if (P (car L))
            (cons (car L) (filter-in P (cdr L)))
            (filter-in P (cdr L))))))

;; Pruebas
(filter-in number? '(a 2 (1 3) b 7))
(filter-in symbol? '(a (b c) 17 foo))
(filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3)))



;; ********************* Problema 5 *********************
;; list-index
;; Propósito:
;; P X L -> int | boolean :  Retorna la posición del primer elemento en
;;                           en la lista que satisface el predicado dado.
;;                           Si ningún elemento satisface el predicado,
;;                           retorna #f
;;
;; list-index-aux
;; Propósito:
;; P X L X int -> int | boolean: Función auxiliar de tipo recursivo de cola
;;                               que lleva conteo de la posición sobre la que
;;                               se está iterando en la lista. Este contador permite
;;                               cubrir funcionalmente el caso en el que algún elemento
;;                               de la lista sí cumpla con el predicado ingresado. 
;;
;; <lista> := ( {<valor-de-scheme>}* )

(define list-index
  (lambda (predicate? list-of-values) 
    (letrec
        ((list-index-aux
          (lambda (pred? l counter)
            (cond
              [(null? l) #f]
              [(pred? (car l)) counter]
              [else (list-index-aux pred? (cdr l) (+ counter 1))]))))
    
    (list-index-aux predicate? list-of-values 0))))

;; Pruebas
(list-index number? '(a 2 (1 3) b 7))
(list-index symbol? '(a (b c) 17 foo))
(list-index symbol? '(1 2 (a b) 3))
(list-index string? '(1 2 3 4 "string" a 10))



;; ********************* Problema 7 *********************
;; cartesian-product
;; Propósito:
;; L1 x L2 -> L' : Retorna una L' de tuplas que representen el producto
;;                 cartesiano entre L1 y L2.
;; <lista> := () | (<SchemeValue> <lista>)

(define cartesian-product
  (lambda (L1 L2)
    (if (null? L1)
        '()
        (if (null? L2)
            '()
            (cons (list (car L1) (car L2))
                  (mi-append (cartesian-product (list (car L1)) (cdr L2))
                          (cartesian-product (cdr L1) L2)))))))
                          
;; Pruebas
(cartesian-product '(a b c) '(x y))
(cartesian-product '(p q r) '(5 6 7))
(cartesian-product '("j" q 1) '(h 7 "n"))

;; zip
;; Propósito:
;; F x L1 x L2 -> L' : Retorna una L' donde la posicion n-ésima corresponde
;;                     al resultado de aplicar la funcion F sobre los elementos en la
;;                     posicion n-esima en L1 y L2.
;; <lista> := () | (<SchemeValue> <lista>)

(define zip
  (lambda (F L1 L2)
    (if (null? L1)
        '()
        (cons (F (car L1)(car L2))(zip F (cdr L1)(cdr L2))))))

;; Pruebas
(zip + '(1 4) '(6 2))
(zip * '(11 5 6) '(10 9 8))
(zip string-append '("Ho" "co" "es") '("la" "mo" "tas"))
(zip eqv? '(a 1 "es") '(a "mo" "es"))

;; operate
;; Propósito:
;; lrators x lrands -> number : Retorna un number de aplicar sucesivamente las operaciones
;;                              en lrators a los valores en lrands.
;; <lrands>  := () | (<number> <lista>)
;; <lrators> := () | (<binSymbol> <lista>)

(define operate
  (lambda (lrators lrands)
    (if (null? lrators)
        (car lrands)
        (operate (cdr lrators)
                 (mi-append (list ((car lrators) (car lrands) (cadr lrands)))(cddr lrands))))))

;; Pruebas
(operate (list + * + - *) '(1 2 8 4 11 6))
(operate (list *) '(4 5))

;; Operar-binarias
;; Propósito:
;; OperacionB -> int : Retorna el int resultante de hacer operaciones suma, resta y multiplicacion
;;                     correspondientes segun la OperacionB.
;;
;; <OperacionB> := <int>
;;              := <OperacionB> ’suma <OperacionB>)
;;              := <OperacionB> ’resta <OperacionB>)
;;              := <OperacionB> ’multiplica <OperacionB>)
(define Operar-binarias
  (lambda (OperacionB)
    (if(number? OperacionB)
       OperacionB
       (cond [(eqv? 'suma (cadr OperacionB)) (+ (Operar-binarias(car OperacionB))(Operar-binarias(caddr OperacionB)))]
             [(eqv? 'resta (cadr OperacionB)) (- (Operar-binarias(car OperacionB))(Operar-binarias(caddr OperacionB)))]
             [(eqv? 'multiplica (cadr OperacionB)) (* (Operar-binarias(car OperacionB))(Operar-binarias(caddr OperacionB)))]))))

;; Pruebas
(Operar-binarias 4)
(Operar-binarias '(2 suma 9))
(Operar-binarias '(2 multiplica 9))
(Operar-binarias '((2 multiplica 3)suma(5 resta 1 )))
(Operar-binarias '((2 multiplica(4 suma 1))multiplica((2 multiplica 4)resta 1)))
