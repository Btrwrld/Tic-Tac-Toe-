#lang racket/gui
;;Comparte todas las funciones con el Gui
(provide (all-defined-out))

#| Genera la matriz de juego, una matriz de unos

   x: Logitud en x de la matriz.
   y: Longitud en y de la matriz.
   matrix: Lista sobre la cual se genera la matriz.

   Ej: (genMatrix 3 3 '())
|#
(define (genMatrix x y matrix)
  (cond ((zero? y)
         matrix)

        (else
         (genMatrix x (- y 1) (cons (genMatrixAux x '()) matrix)))
        )
  )
(define (genMatrixAux x res)
  (cond ((zero? x)
        res)

        (else
         (genMatrixAux (- x 1) (cons 1 res)))
        )
  )



#| Modifica el punto (x,y) de la matriz de juego. La matriz es 0 indexada

   x: Columna de la matriz.
   y: Fila de la matriz.
   user: Marcas "X" o "O" a plasmar en la matriz
   matrix: Matriz a modificar.

   Ej: (makeMove 0 0 "X" (list (list 1 1 1) (list 1 1 1) (list 1 1 1)))
|#
(define (makeMove x y user matrix)
  (cond ((zero? y)
         (append (list (makeMoveAux x user (car matrix))) (cdr matrix)))

        (else
         (append (list(car matrix)) (makeMove x (- y 1) user (cdr matrix))))
        )
  )
(define (makeMoveAux x user row)
  (cond ((zero? x)
         (append (list user) (cdr row)))

        (else
         (append (list (car row)) (makeMoveAux (- x 1) user (cdr row))))
        )
  )



#| Evalua el punto (x,y) de la matriz de juego y regresa si es un
   movimiento permitido, es decir si el campo esta vacio.

   x: Columna de la matriz.
   y: Fila de la matriz.
   matrix: Matriz a evaluar.

   Ej: (validMove? 0 0 (list (list 1 1 1) (list 1 1 1) (list 1 1 1)))
|#
(define (validMove? x y matrix)
  (cond ((zero? y)
         (validMoveAux x (car matrix)))

        (else
         (validMove? x (- y 1) (cdr matrix)))
        )
  )
(define (validMoveAux x row)
  (cond ((and (zero? x) (equal? (car row) 1))
         #t)

        ((> x 0)
         (validMoveAux (- x 1) (cdr row)))

        (else
        #f)
        )
  )

#| Obtiene la longitud en x de la matriz

   matrix: Matriz de juego.

   Ej: (matX (list (list 1 1 1) (list 1 1 1) (list 1 1 1)))
|#
(define (matX matrix)
  (cond ((null? matrix)
         0)
        (else
          (matXAux (car matrix)))
        )
  )
(define (matXAux matrix)
  (cond ((null? matrix)
         0)
        (else
         (+ 1 (matXAux (cdr matrix)))
         )
        )
  )
#| Obtiene la longitud en y de la matriz

   matrix: Matriz de juego.

   Ej: (matY (list (list 1 1 1) (list 1 1 1) (list 1 1 1)))
|#
(define (matY matrix)
  (cond ((null? matrix)
         0)
        (else
         (+ 1 (matY (cdr matrix))))
        )
  )

#| Regresa el valor de la menor dimensión de la matriz x o y

   matrix: La matriz del juego

|#
(define (minMat matrix)
  (cond ((> (matX matrix) (matY matrix))
         (matY matrix))
        
        ((<= (matX matrix) (matY matrix))
         (matX matrix))
   )
  )



#| Ejecuta el algoritmo para el turno del sistema

   matrix: Matriz de juego.

   Ej: (myTurn (list (list "X" "X" 1) (list 1 "O" 1) (list 1 1 1)))
|#
(define (myTurn matrix)
  (cond ((equal? (spacesToWin matrix "X") 1)
          (play matrix "X" 1))

        ((equal? (spacesToWin matrix "O") 1)
          (play matrix "O" 1))

        (else
         (play matrix "X" (spacesToWin matrix "X")))
    ) 
  )






(define (play matrix player toWin)
   (cond ((equal? (bestSolution matrix player) "rows")
         (winRow matrix player toWin 0))

         ((equal? (bestSolution matrix player) "columns")
         (winColumn matrix player toWin))

          ((equal? (bestSolution matrix player) "diagonal")
         (winDiagonal matrix player toWin))

         ((equal? (bestSolution matrix player) "tie")
         (freePick matrix 0))
      ) 
  )

#|Marca el primer elemento que encuentre disponible

   matrix: Matriz de juego.
   player: juagor a evaluar

   Ej: (winRow (list (list "X" "X" 1) (list 1 "X" 1) (list 1 1 1)) "X" 0)
|#
(define (freePick matrix y)
 (cond  ((equal? -1 (freePickAux (car matrix) 0))
         (freePick (cdr matrix) (+ 1 y)))
        
        (else
         (list (freePickAux (car matrix) 0) y))
        )
  )
(define (freePickAux row x)
 (cond ((null? row)
        -1)
   
        ((equal? 1 (car row))
         x)
        
        (else
         (freePickAux (cdr row) (+ 1 x)))
        )
  )


#| Obtiene la posición para ganar la fila

   matrix: Matriz de juego.
   player: juagor a evaluar
   y: numero de columna inicial

   Ej: (winRow (list (list "X" "X" 1) (list 1 "X" 1) (list 1 1 1)) "X" 0)
|#
(define (winRow matrix player toWin y)
  (cond ((equal? toWin (checkRow (car matrix) player))
         (list (winRowAux (car matrix) player 0) y))

        (else
         (winRow (cdr matrix) player toWin (+ 1 y)))
    )
  )
(define (winRowAux matrix player x)
  (cond ((equal? 1 matrix)
         x)

        ((equal? 1 (car matrix))
         x)

        (else
         (winRowAux (cdr matrix) player (+ 1 x)))
    )
  )



#| Obtiene la posición para ganar la columna

   matrix: Matriz de juego.
   player: juagor a evaluar

   Ej: (winColumn (list (list "X" "X" 1) (list "X" 1 1) (list 1 1 1)) "X")
|#
(define (winColumn matrix player toWin)
  (list (cadr (winRow (transpose matrix) player toWin 0))  (car (winRow (transpose matrix) player toWin 0)))
  )


#| Obtiene la posición para ganar la diagonal derecha

   matrix: Matriz de juego.
   player: juagor a evaluar
   x : Posicion inicial en x
   y : Posicion inicial en y

   Ej: (winRightDiagonal (list (list "X" "X" 1) (list 1 "X" 1) (list 1 1 1)) "X" 0 0)
|#
(define (winRightDiagonal matrix player toWin x y)
  (cond ((equal? toWin (toWinDiagonalAux matrix player))
         (winDiagonalAux matrix player x y))
        
        (else
         (winRightDiagonal (cdr matrix) player toWin x (+ 1 y)))
    )
  )
(define (winDiagonalAux matrix player x y)
  (cond ((equal? 1 (caar matrix))
         (list x y))
        
        (else
         (winDiagonalAux (cdr(remove_column matrix)) player (+ 1 x) (+ 1 y)))
    )
  )


#| Obtiene la posición para ganar la diagonal izq

   matrix: Matriz de juego.
   player: juagor a evaluar

   Ej: (winLeftDiagonal (list (list 1 1 "X") (list 1 "X" 1) (list 1 1 1)) "X")
|#
(define (winLeftDiagonal matrix player toWin)
  (list (car (winRightDiagonal (invert matrix) player toWin 0 0)) (- (- (matY matrix) 1) (cadr (winRightDiagonal (invert matrix) player toWin 0 0))))
  )



#| Obtiene la posición para ganar cualquier diagonal

   matrix: Matriz de juego.
   player: juagor a evaluar

   Ej: (winLeftDiagonal (list (list 1 1 "X") (list 1 "X" 1) (list 1 1 1)) "X")
|#
(define (winDiagonal matrix player toWin)
  (cond ((equal? (toWinRightDiagonal matrix player (minMat matrix) (minMat matrix)) toWin)
         (winRightDiagonal matrix player toWin 0 0))

        ((equal? (toWinRightDiagonal (transpose matrix) player (minMat (transpose matrix)) (minMat (transpose matrix))) toWin)
         (list (cadr (winRightDiagonal (transpose matrix) player toWin 0 0)) (car (winRightDiagonal (transpose matrix) player toWin 0 0)) ))

        ((equal? (toWinLeftDiagonal matrix player) toWin)
         (winLeftDiagonal matrix player toWin))

         ((equal? (toWinLeftDiagonal (transpose matrix) player) toWin)
         (list (cadr (winLeftDiagonal (transpose matrix) player toWin)) (car (winLeftDiagonal (transpose matrix) player toWin)) ))
    )
  )


#| Retorna el mínimo número de movimientos para ganar

   matrix: La matriz del juego
   player: El elemento a comparar

   Ej: (spacesToWin (list (list "X" "X" 1) (list 1 "O" 1) (list 1 1 1)) "X")
|#

(define (spacesToWin matrix player)
  (cond ((equal? (bestSolution matrix player) "rows")
         (toWinRow matrix player (matX matrix)))

         ((equal? (bestSolution matrix player) "columns")
         (toWinColumn matrix player (matY matrix)))
      
         ((equal? (bestSolution matrix player) "diagonal")
         (toWinDiagonal matrix player))
      )
  )

#| Elige al mejor candidato para la solucion, puede ser una fila, columna o diagonal
   Funcion de seleccion 

   matrix: La matriz del juego
   player El jugador
  
|#
(define (bestSolution matrix player)
  (cond ((and (<= (toWinRow matrix player (matX matrix)) (toWinColumn matrix player (matY matrix)))
              (<= (toWinRow matrix player (matX matrix)) (toWinDiagonal matrix player))     
              (viableRows? matrix player))
         "rows")

         ((and (<= (toWinColumn matrix player (matY matrix)) (toWinRow matrix player (matX matrix)))
               (<= (toWinColumn matrix player (matY matrix)) (toWinDiagonal matrix player))
               (viableColumns? matrix player))
         "columns")

          ((and (<= (toWinDiagonal matrix player) (toWinRow matrix player (matX matrix)))
                (<= (toWinDiagonal matrix player) (toWinColumn matrix player (matY matrix)))
                (viableDiagonal? matrix player (minMat matrix)))
         "diagonal") 

        (else
         "tie")
      ) 
  )

#| Determina si es posible ganar alguna fila

   matrix: La matriz del juego
   player El jugador
|#
(define (viableRows? matrix player)
  (cond ((null? matrix)
        #f)
        
        ((<= 11 (checkRow (car matrix) player))
         (viableRows? (cdr matrix) player))
        
        (else
         #t)
        )
  )
#| Determina si es posible ganar alguna columna

   matrix: La matriz del juego
   player El jugador
|#
(define (viableColumns? matrix player)
  (viableRows? (transpose matrix) player)
  )
#| Determina si es posible ganar alguna diagonal derecha

   matrix: La matriz del juego
   player El jugador
|#
(define (viableDiagonal? matrix player lim)
  (or (viableRDiagonal? matrix player (minMat matrix)) (viableRDiagonal? (transpose matrix) player (minMat (transpose matrix)))
        (viableLDiagonal? matrix player) (viableLDiagonal? (transpose matrix) player))
  )
(define (viableRDiagonal? matrix player lim )
  (cond ((or (null? matrix) (> lim (minMat matrix)))
         #f)
        
        ((and (<= 11 (toWinDiagonalAux matrix player)) (<= lim (minMat matrix)))
         (viableRDiagonal? (cdr matrix) player lim))

        (else
         #t)
        )
  )

  #| Determina si es posible ganar alguna diagonal izquierda

   matrix: La matriz del juego
   player El jugador
|#
(define (viableLDiagonal? matrix player)
  (viableRDiagonal? (invert matrix) player (minMat (invert matrix)))
  )






#| La cantidad de pasos que hacen falta para ganar la fila
   Funcion de viabilidad

   matrix: La matriz del juego
   player El jugador
   num: numero minimo de movimientos para gana la fila
|#

(define (toWinRow matrix player num)
  (cond ((null? matrix)
        num)
        
        ((> num (checkRow (car matrix) player))
         (toWinRow (cdr matrix) player (checkRow (car matrix) player)))
        
        (else
         (toWinRow (cdr matrix) player num))
        )
  )

#| Cuenta la cantidad de espacios necesarios para ganar la lista

   lista: Lista de valores a comparar
   player El elemento a comparar
|#

(define (checkRow lista player)
  (cond ((null? lista)
         0)
        
       ((equal? (car lista) player)
        (checkRow (cdr lista) player))

        ((not (equal? (car lista) 1))
        11)
       
       (else
        (+ 1 (checkRow (cdr lista) player)))
       )
  )



#| La cantidad de pasos que hacen falta para ganar la columna
   Funcion de viabilidad

   matrix: La matriz del juego
   player El elemento a comparar
   num: numero minimo de movimientos para gana la columna
|#

(define (toWinColumn matrix player num)
  (toWinRow (transpose matrix) player num)
  )



#| Cuanta la cantidad de movimientos para ganar una diagonal si ya hay una marca del
   oponente suma 11 para que no se tome en cuenta

   matrix: La matriz del juego
   player: El elemento a comparar
|#

(define (toWinDiagonalAux matrix player)
  (cond ((or (null? matrix) (null? (car matrix)))
        0)

       ((and (equal? (caar matrix) player) (not(null? (cdar matrix))) (null? (cdr matrix)) )
        0)
       
       ((and (not(equal? (caar matrix) player)) (not(null? (cdar matrix))) (null? (cdr matrix)) )
        1)
       
       ((equal? (caar matrix) player)
        (toWinDiagonalAux (cdr(remove_column matrix)) player))

       ((equal? (caar matrix) 1)
        (+ 1 (toWinDiagonalAux (cdr(remove_column matrix)) player)))

       ((not (equal? (caar matrix) 1))
        11)
       
       (else
        0)
       
       )
  )

#| Obtiene el menor numero de movimientos necesarios para ganr la diagonal izq
   Funcion de viabilidad

   matrix: La matriz del juego
   player: El elemento a comparar
   num: numero minimo de movimientos para ganar la diagonal
|#

(define (toWinRightDiagonal matrix player num lim)
  (cond ((null? matrix)
         num)
        
        ((and (> num (toWinDiagonalAux matrix player)) (<= lim (minMat matrix)))
         (toWinRightDiagonal (cdr matrix) player (toWinDiagonalAux matrix player) lim))
        
        ((<= lim (minMat matrix))
         (toWinRightDiagonal (cdr matrix) player num lim))

        (else
         num)

        )
  )

#| Obtiene el menor numero de movimientos necesarios para ganr la diagonal izq
   Funcion de viabilidad

   matrix: La matriz del juego
   player: El elemento a comparar
   num: numero minimo de movimientos para gana la diagonal

|#
(define (toWinLeftDiagonal matrix player)
  (toWinRightDiagonal (invert matrix) player (minMat matrix) (minMat matrix))
  )


#| Obtiene el menor numero de movimientos necesarios para ganar cualquier diagonal 
   Funcion de viabilidad

   matrix: La matriz del juego
   player: El elemento a comparar
   num: numero minimo de movimientos para gana la diagonal

|#
(define (toWinDiagonal matrix player)
  (min (toWinRightDiagonal matrix player (minMat matrix) (minMat matrix)) (toWinRightDiagonal (transpose matrix) player (minMat matrix) (minMat matrix))
        (toWinLeftDiagonal matrix player) (toWinLeftDiagonal (transpose matrix) player))
  )




#| Remueve la primera columna de una matriz dada

   matrix: La matriz del juego
|#

(define (remove_column matrix)
  (cond((null? matrix) '())
       (else(cons (cdar matrix) (remove_column (cdr matrix))))
       )
  )

#| Retorna solo la primera columna

   matrix: La matriz del juego
|#

(define (get_column matrix)
  (cond((null? matrix) '())
       (else(cons (caar matrix) (get_column (cdr matrix))))))

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
 




;; Pruebas para hacerlo caer 1m1

  ;;(list (list "X" "O" 1 1 "X") (list "O" "O" 1 1 "X") (list "O" "O" 1 1 "X") (list 1 "X" 1 1 "O") (list "X" 1 1 1 "X")) "O"


  
;;(diagonal_1 (list (list 1 1 1) (list "X" 1 1) (list 1 "X" 1) (list 1 1 1)) "X" (minMat (list (list 1 1 1) (list "X" 1 1) (list 1 "X" 1) (list 1 1 1))) (minMat (list (list 1 1 1) (list "X" 1 1) (list 1 "X" 1) (list 1 1 1))))
  