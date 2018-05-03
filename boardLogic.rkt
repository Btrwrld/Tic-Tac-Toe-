;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname boardLogic) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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