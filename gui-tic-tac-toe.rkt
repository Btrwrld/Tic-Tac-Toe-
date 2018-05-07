#lang racket/gui
;Importa las definiciones de boardLogic
(require "boardLogic.rkt")


;Define vector para el horizontal-panel
(define vec-fila vector)
(define vec-boton vector)
(define matriz-juego list)

;click-listener del boton
(define (on-button-click button index1 index2 event)
  (send button enable #f)
  (send button set-label "X")
  (set! matriz-juego (makeMove index1 index2 "X" matriz-juego))
  (printf "button ~a ~b clicked~%" index1 index2))

;Funcion utilizada para que se observe el movimiento de la computadora en el GUI
(define (plot-pc index1 index2 vect)
  (send (vector-ref (vector-ref vect index2) index1) set-label "O"))

;Crear una ventana
(define toplevel (new frame% [label "Gui-Tic-Tac-Toe"]))

;Crear un panel horizontal para cada fila
(define (new-panel fila)
   (for/vector ([k(in-range fila)])
     (new horizontal-panel% [parent toplevel])))

;Setear valores a los vectores
(define (buttons cols fils)
  (set! vec-fila (new-panel fils))
  (set! vec-boton (buttons_aux cols fils )))
  
 ;Funcion Para dibujar los botones
(define (buttons_aux columnas filas)
  (for/vector ([i(in-range columnas)])
    (for/vector ([j(in-range filas)])
      (new button%
           [parent (vector-ref vec-fila i)]
           [label ""]
           [min-width 50]
           [min-height 50]
           [stretchable-width #f]	 
           [stretchable-height #f]
           [callback (lambda (button event) (on-button-click button i (bin-dec j) event))]))))

;Funcion de validacion (filas > 1 y columnas > 2)
(define (validacion n1 n2)
  (cond ((and (>= n1 3) (>= n2 3) (<= n1 10) (<= n2 10)) #t)
        (else #f)))
  
;Funcion llamada para dibujar la matriz
(define (TTT columnas filas)
  (cond ((equal? #t (validacion columnas filas)) (buttons columnas filas) (send toplevel show #t) (set! matriz-juego (genMatrix filas columnas '())))
        (else
         "Debe introducir valores validos para la matriz")))

;Funcion para convertir de binario a decimal
(define (bin-dec n)
  (cond ((zero? n) n)
        (else
      (+ (modulo n 10) (* 2 (bin-dec (quotient n 10)))))))