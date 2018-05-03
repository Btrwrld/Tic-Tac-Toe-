#lang racket/gui

;Define vector para el horizontal-panel
(define vec-fila vector)
(define vec-boton vector)

;click-listener del boton
(define (on-button-click button index1 index2 event)
  (send button enable #f)
  (send button set-label "X")
  (printf "button ~a ~b clicked~%" index1 index2))

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
           [callback (lambda (button event) (on-button-click button i j event))]))))

;Funcion de validacion (filas > 1 y columnas > 2)
(define (validacion n1 n2)
  (cond ((and (> n1 2) (> n2 1)) #t)
        (else #f)))
  
;Funcion llamada para dibujar la matriz
(define (draw columnas filas)
  (cond ((equal? #t (validacion columnas filas)) (buttons columnas filas) (send toplevel show #t))
        (else
         "Debe introducir valores validos para la matriz")))