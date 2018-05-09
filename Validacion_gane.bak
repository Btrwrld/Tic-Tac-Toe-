;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Validación de gane|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
#| Retorna quien ganó ("X" ó "O")

   matrix: La matriz del juego
|#
(define (who_win? matrix)
  (cond((win? matrix "X") "X")
       ((win? matrix "O") "O")
       ((not(and (win? matrix "X") (win? matrix "O"))) "Ninguno ganó")))

#| Retorna si ganó ("X" ó "O")

   matrix: La matriz del juego
   num: El elemento a comparar
|#

(define (win? matrix num)
  (or (rows matrix num) (columns matrix num)
      (diagonal_todas matrix num)))
       

#| Traspone la matriz

   matrix: La matriz del juego
|#

(define (transpose matrix)
  (cond((null? matrix) '())
       ((null? (car matrix)) '())
       (else( cons (get_column matrix) (transpose (remove_column matrix))))))

#| Remueve la primera columna de una matriz dada

   matrix: La matriz del juego
|#

(define (remove_column matrix)
  (cond((null? matrix) '())
       (else(cons (cdar matrix) (remove_column (cdr matrix))))))

#| Valida si hay una fila llena del elemento a comparar("X" ó "O")

   matrix: La matriz del juego
   num: El elemento a comparar
|#

(define (rows matrix num)
  (cond((null? matrix)#f)
        ((verify_list (car matrix) num) #t)
        (else(rows (cdr matrix) num))))

#| Valida si hay una columna llena del elemento a comparar("X" ó "O")

   matrix: La matriz del juego
   num: El elemento a comparar
|#

(define (columns matrix num)
  (rows (transpose matrix) num))

#| Retorna solo la primera columna

   matrix: La matriz del juego
|#

(define (get_column matrix)
  (cond((null? matrix) '())
       (else(cons (caar matrix) (get_column (cdr matrix))))))

#| Verifica si un elemento determinado está en toda la lista dada

   lista: Lista de valores a comparar
   num: El elemento a comparar
|#

(define (verify_list lista num)
  (cond((null? lista)#t)
       ((equal? (car lista) num) (verify_list (cdr lista) num))
       (else #f)))

#| Si lo que se le pasa es una lista, la función la invierte y si es una matriz lo que hace es
   invertir la diagonal

   lista:Puede ser una lista ó una matriz 

   Ejemplo: Matriz sin aplicar la función:  (4 5 6)  aplicando la función: (7 8 9)
                                            (1 2 3)                        (1 2 3)
                                            (7 8 9)                        (4 5 6)
                                           
|#

(define (invert lista )
  (cond((null? lista) '())
       (else( append (invert (cdr lista))  (list(car lista)) ))))

#| Valida si hay una diagonal(\) llena del elemento a comparar("X" ó "O")

   matrix: La matriz del juego
   num: El elemento a comparar
|#

(define (diagonal matrix num)
  (cond((null? matrix)#t)
       ((null? (car matrix)) #t)
       ((and (equal? (caar matrix) num) (not(null? (cdar matrix))) (null? (cdr matrix)) ) #f)
       ((equal? (caar matrix) num) (diagonal (cdr(remove_column matrix)) num) )
       (else #f)))

#| Valida si hay una diagonal(\) llena del elemento a comparar("X" ó "O"),en esta su función es muy similar al
   anterior, nada más que se agrega una validación para que no solo sirva con matrices cuadradas

   matrix: La matriz del juego
   num: El elemento a comparar
|#

(define (diagonal_1 matrix num)
  (cond ((null? matrix)#f)
        ((diagonal matrix num) #t)
        (else (diagonal_1 (cdr matrix) num))))

#| Valida si hay una diagonal(/) llena del elemento a comparar("X" ó "O")

   matrix: La matriz del juego
   num: El elemento a comparar
|#
(define (diagonal_2 matrix num)
  (diagonal_1 (invert matrix) num))
  
 #| Valida todas las diagonales

   matrix: La matriz del juego
   num: El elemento a comparar
|#

(define (diagonal_todas matrix num)
  (or (diagonal_1 matrix num) (diagonal_2 matrix num)
      (diagonal_1 (transpose matrix) num) (diagonal_2 (transpose matrix) num)))
 
 
 
