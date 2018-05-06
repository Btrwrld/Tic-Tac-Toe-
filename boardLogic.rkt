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

#|
#| Ejecuta el algoritmo para el turno del sistema

   matrix: Matriz de juego.

   Ej: (myTurn (list (list "X" "X" 1) (list 1 "O" 1) (list 1 1 1)))
|#
(define (myTurn matrix)
  (cond ((not (null? (winningChances matrix (matX (car matrix)) (matY matrix) )))
         (makeMove (car (winningMove matrix)) (cadr (winningMove matrix))) )
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
         (+ 1 (matX (cdr matrix))))
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


#| Obtiene una lista con todas la posibilidades de ganar en el eje X, Y o en diagonal

   matrix: Matriz de juego.

   Ej: (matY (list (list 1 1 1) (list 1 1 1) (list 1 1 1)))
|#
(define (winningChances matrix user)
  (cond ((not (null? (xRun matrix user 0 0)))
         (xRun matrix user 0 0))

        ((not (null? (yRun matrix user 0 0)))
         (yRun matrix user 0 0))

        ((not (null? (dRun matrix user 0 0)))
         (dRun matrix user 0 0))

        (else '())
        )
  )


#| Obtiene una lista con todas la posibilidades de ganar en el eje X
   esto mediante un barrido horizontal de la matriz

   matrix: Matriz de juego.
   user: Usuario del que se buscan las posibilidades de ganar
   x: Posición inicial en el eje x
   y: Posición inicial en el eje y

   Ej: (xRun (list (list "O" "O" 1) (list 1 1 1) (list 1 1 1)) x y) 
|#
(define (xRun matrix user x y)
  (cond ((null? matrix) '())

        ()
        )
  )

#| Obtiene una lista con todas la posibilidades de ganar en el eje y
   esto mediante un barrido vertical de la matrix

   matrix: Matriz de juego.
   user: Usuario del que se buscan las posibilidades de ganar
   x: Posición inicial en el eje x
   y: Posición inicial en el eje y

   Ej: (xRun (list (list "O" 1 1) (list "O" 1 1) (list 1 1 1)) x y) 
|#
(define (yRun matrix user x y)
  (cond ((null? matrix) '())

        ()
        )
  )

#| Obtiene una lista con todas la posibilidades de ganar en el eje diagonal
   esto mediante un barrido diagonal de la matriz

   matrix: Matriz de juego.
   user: Usuario del que se buscan las posibilidades de ganar
   x: Posición inicial en el eje x
   y: Posición inicial en el eje y

   Ej: (xRun (list (list "O" "O" 1) (list 1 "O" 1) (list 1 1 1)) x y) 
|#
(define (dRun matrix user x y)
  (cond ((null? matrix) '())

        ()
        )
  )
|#

