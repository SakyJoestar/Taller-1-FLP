;; Nombre
;; Samuel Galindo 2177491
;; Nombre
;; Nicolas Herrera 2182551

#lang eopl

;; mi-append
;; Proposito:
;; L1 x L2 -> L' : Retorna una lista que contiene los elementos que pertenecen a L1 y L2..
;;
;; <lista>      := () | (<SchemeValue> <lista>)

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

;; invert:
;; Proposito:
;; L -> L' : Invierte los pares de L.
;;
;; <lista> ::= () | (<par> <lista>)
;; <par>   ::= (<elemento> <elemento>)
;; <elemento>   ::= <number> | <symbol> | <string>

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

;; filter-in:
;; Proposito:
;; P x L -> L' : Retorna una lista que contiene los elementos que pertenecen a L y que satisfacen el predicado P
;;
;; <lista> ::= () | (<elemento> <lista>)
;; <elemento>   ::= <number> | <symbol> | <string>

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

;; cartesian-product
;; Proposito:
;; L1 x L2 -> L' : Retorna una L' de tuplas que representen el producto cartesiano entre L1 y L2.
;;
;; <lista> ::= () | (<elemento> <lista>)
;; <elemento>   ::= <number> | <symbol> | <string>

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

;; cartesian-product
;; Proposito:
;; F x L1 x L2 -> L' : Retorna una L' donde la posicion n-esima corresponde al resultado de aplicar la funcion F sobre los elementos en la posicion n-esima en L1 y L2.
;;
;; <lista> ::= () | (<elemento> <lista>)
;; <elemento>   ::= <number> | <symbol> | <string>

(define zip
  (lambda (F L1 L2)
    (if (null? L1)
        '()
        (cons (F (car L1)(car L2))(zip F (cdr L1)(cdr L2))))))

;; Pruebas
(zip + '(1 4) '(6 2))
(zip * '(11 5 6) '(10 9 8))

;; operate
;; Proposito:
;; lrators x lrands -> number : Retorna un number de aplicar sucesivamente las operaciones en lrators a los valores en lrands.
;;
;; <lrands> ::= () | (<number> <lista>)
;; <lrators> ::= () | (<symbol> <lista>)

(define operate
  (lambda (lrators lrands)
    (if (null? lrators)
        (car lrands)
        (operate (cdr lrators)
                 (mi-append (list ((car lrators) (car lrands) (cadr lrands)))(cddr lrands))))))

;; Pruebas
(operate (list + * + - *) '(1 2 8 4 11 6))
(operate (list *) '(4 5))

;; cartesian-product
;; Proposito:
;; OperacionB -> int : Retorna el int de hacer operaciones suma, resta y multiplicacion correspondientes segun la OperacionB.
;;
;; <OperacionB> ::= <int>
;;              ::= <OperacionB> ’suma <OperacionB>)
;;              ::= <OperacionB> ’resta <OperacionB>)
;;              ::= <OperacionB> ’multiplica <OperacionB>)
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
