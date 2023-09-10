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
;; L1 x L2 -> L' : Retorna una lista que contiene los elementos
;;                 que pertenecen a L1 y L2.
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
;;
;; <lista> := ( {<SchemeValue>}* )

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
;;
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
;;
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



;; ********************* Problema 8 *********************
;; mapping
;; Propósito:
;; F x L1 X L2 -> L : Retorna una lista L de pares (a, b), donde a es un
;;                    elemento de L1 y b es un elemento de L2, tal que se
;;                    cumple que F(a) = b.
;;                    a y b son elementos en la misma posición en L1 y L2.
;;
;; <lista> := ( {<int>}* )

(define mapping
  (lambda (function list1 list2)
    (cond
      [(null? list1)
       empty]
      [(eqv? (function (car list1)) (car list2))
       (cons (list (car list1) (car list2)) (mapping function (cdr list1) (cdr list2)))]
      [else
       (mapping function (cdr list1) (cdr list2))])))

;; Pruebas
(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6))
(mapping (lambda (d) (* d 3)) (list 1 2 2) (list 2 4 6))
(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 3 9 12))
(mapping (lambda (d) (* d 10)) (list 1 2 3) (list 20 10 30))



;; ********************* Problema 10 *********************
;; up
;; Propósito:
;; L -> L': Retorna una lista L' donde son removidos los paréntesis de los elementos
;;          que están en el nivel más alto de la lista original L.
;;          Si un elemento del nivel más alto de L no tiene paréntesis, es incluido
;;          en L' sin sufrir ninguna modificación.
;;
;; <lista> := ( {<SchemeValue>}* )

(define up
  (lambda (list-of-values)      
      (cond
        [(null? list-of-values)
         empty]
        [(list? (car list-of-values))
         (mi-append (car list-of-values) (up (cdr list-of-values)))]
        [else
         (cons (car list-of-values) (up (cdr list-of-values)))])))

;; Pruebas
(up '((1 2) (3 4)))
(up '((x (y)) z))
(up '(1 2 3 4 (a b ((c)) 10) x (y) (((z))) "fin"))


      
;; ********************* Problema 11 *********************
;; zip
;; Propósito:
;; F x L1 x L2 -> L' : Retorna una L' donde la posicion n-ésima corresponde
;;                     al resultado de aplicar la funcion F sobre los elementos
;;                     en la posicion n-ésima en L1 y L2.
;;
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



;; ********************* Problema 13 *********************
;; operate
;; Propósito:
;; lrators x lrands -> number : Retorna el número resultante de aplicar sucesivamente
;;                              las operaciones en lrators a los valores en lrands.
;;
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



;; ********************* Problema 14 *********************
;; path
;; Propósito:
;; n X tree -> L : Retorna el camino que se debe seguir en el árbol binario de búsqueda
;;                 ingresado, para llegar hasta el número entero n. Este camino es una lista
;;                 de cadenas, donde se indica la dirección a tomar en cada nodo: <-left o right->.
;;                 Si el número n se encuentra en la raíz del árbol, se devuelve la lista vacía.
;;                 El árbol debe contener el número n.
;;
;; <árbol-binario-búsqueda> := ()
;;                          := ( <int> <árbol-binario-búsqueda> <árbol-binario-búsqueda> )

(define path
  (lambda (n tree)
    (cond
      [(eqv? n (car tree))
       empty]
      [(< n (car tree))
       (cons 'left (path n (cadr tree)))]
      [else
       (cons 'right (path n (caddr tree)))])))

;; Pruebas
(path 17 '(14 (7 () (12 () ())) (26 (20 (17 () ()) ()) (31 () ()))))
(path 13 '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))
(path 2 '(2 (1 () ()) (3 () ())))



;; ********************* Problema 16 *********************
;; Operar-binarias
;; Propósito:
;; OperacionB -> int : Retorna el número resultante de aplicar operaciones suma, resta y multiplicacion
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



;; ********************* Problema 17 *********************
;; prod-scalar-matriz
;; Propósito:
;; mat X vec -> mat' : Retorna la matriz resultante de multiplicar término a término 
;;                     cada fila de la matriz de entrada con el vector ingresado.
;;                     El número de columnas en la matriz debe ser igual que la
;;                     longitud del vector.
;;
;; prod-vec-aux
;; Propósito:
;; vec1 X vec2 -> vec' : Retorna el vector resultante de multiplicar término a término
;;                       los dos vectores ingresados como parámetro.
;;
;; <matriz> := ( {<vector>}+ ) 
;; <vector> := ( {<int>}+ )
;; Restricción: la longitud de los vectores en una matriz debe ser la misma. 

(define prod-scalar-matriz
  (lambda (mat vec)
    (letrec
     ((prod-vec-aux
       (lambda (vector1 vector2)
         (if (null? vector1)
             empty
             (cons
              (* (car vector1) (car vector2))
              (prod-vec-aux (cdr vector1) (cdr vector2)))))))

      (if (null? mat)
          empty
          (cons
           (prod-vec-aux (car mat) vec)
           (prod-scalar-matriz (cdr mat) vec))))))

;; Pruebas
(prod-scalar-matriz '((1 1) (2 2)) '(2 3))
(prod-scalar-matriz '((1 1) (2 2) (3 3)) '(2 3))
