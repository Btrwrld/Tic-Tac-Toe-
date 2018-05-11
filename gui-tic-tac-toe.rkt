#lang racket/gui
;Importa las definiciones de boardLogic
(require "boardLogic.rkt")

;Define vector para el horizontal-panel
(define vec-fila vector)

;Define un vector para guardar los botones
(define vec-boton vector)

;Define una lista para guardar la matriz del juego
(define matriz-juego list)

;Crear una ventana para el juego
(define toplevel (new frame% [label "Gui-Tic-Tac-Toe"]))

;Hacer una ventana para el mensaje de hay ganador
(define frame (new frame% [label "Ganador"]))

;click-listener del boton
(define (on-button-click button index1 index2 event)
  (cond ((equal? (win? matriz-juego "X") #f)
        (send button enable #f)
        (send button set-label "O")
        (send button set-color "black")
        (set! matriz-juego (makeMove index2 index1 "O" matriz-juego))
        (plot-aux matriz-juego vec-boton)
        (mensaje "El jugador X es el ganador" "X"))
        (else
         (mensaje "El jugador X es el ganador" "X"))))

;Funcion utilizada para que se observe el movimiento de la computadora en el GUI
(define (plot-pc list-index vect)
  (cond ((equal? (win? matriz-juego "O") #f)
         (send (vector-ref (vector-ref vect (cadr list-index)) (car list-index)) set-label "X")
         (send (vector-ref (vector-ref vect (cadr list-index)) (car list-index)) set-color "red")
         (set! matriz-juego (makeMove (car list-index) (cadr list-index) "X" matriz-juego))
         (send (vector-ref (vector-ref vect (cadr list-index)) (car list-index)) enable #f)
         (plot-aux2 matriz-juego))
        (else
         (mensaje "El jugador O es el ganador" "O"))))
       

(define (plot-aux matriz vector)
  (cond ((equal? (miembro matriz 1) #f) (mensaje2 "El juego a quedado empatado"))
  (else
   (plot-pc (myTurn matriz) vector) '())))

(define (plot-aux2 matriz)
  (cond ((equal? (miembro matriz 1) #f) (mensaje2 "El juego a quedado empatado"))
  (else #f)))

;Funcion para imprimir mensaje de ganador
;Mensaje en pantalla
(define (mensaje texto who)
   (cond ((equal? (win? matriz-juego who) #t)
          (define msg1 (new message% [parent frame]
                            [label texto]
                            [font (make-object font% 50 'default 'normal 'bold)]))
           (new button%
           [parent frame]
           [label "Clear"]
           [min-width 90]
           [min-height 50]
           [stretchable-width #f]	 
           [stretchable-height #f]
           [font (make-object font% 10 'default 'normal 'bold)]
           [callback (lambda (button event) (on-button2-click button event))])
          (send frame show #t))

         (else #f)))

;Funcion para imprimir mensaje de empate
;Mensaje en pantalla
(define (mensaje2 texto)
          (define msg1 (new message% [parent frame]
                            [label texto]
                            [font (make-object font% 50 'default 'normal 'bold)]))
           (new button%
           [parent frame]
           [label "Clear"]
           [min-width 90]
           [min-height 50]
           [stretchable-width #f]	 
           [stretchable-height #f]
           [font (make-object font% 10 'default 'normal 'bold)]
           [callback (lambda (button event) (on-button2-click button event))])
           (send frame show #t))

;Funcion para el boton de clear
(define (on-button2-click button event)
  (set! matriz-juego '())
  (set! vec-boton '())
  (set! vec-fila '())
  (delete-children toplevel)
  (delete-children frame)
  (send toplevel show #f)
  (send frame show #f))
  
  

;Funcion para determinar si un elemento pertenece a una matriz
(define (miembro matriz ele)
  (cond ((null? matriz) #f)
        ((equal? (miembro-aux (car matriz) ele) #t) #t)
        (else
         (miembro (cdr matriz) ele))))

  
(define (miembro-aux matriz ele)
  (cond ((null? matriz) #f)
        ((equal? ele (car matriz)) #t)
        (else
         (miembro-aux (cdr matriz) ele))))
  

;Crear un panel horizontal para cada fila
(define (new-panel fila)
   (for/vector ([k(in-range fila)])
     (new horizontal-panel% [parent toplevel])))

;Setear valores a los vectores
(define (set-vals cols fils)
  (set! vec-fila (new-panel fils))
  (set! vec-boton (buttons-gen fils cols)))
  
 ;Funcion Para dibujar los botones
(define (buttons-gen columnas filas)
  (for/vector ([i(in-range columnas)])
    (for/vector ([j(in-range filas)])
      (new colorable-button%
           [parent (vector-ref vec-fila i)]
           [label ""]
           [min-width 90]
           [min-height 90]
           [stretchable-width #f]	 
           [stretchable-height #f]
           [font (make-object font% 50 'default 'normal 'bold)]
           [callback (lambda (button event) (on-button-click button i (bin-dec j) event))]))))

;Funcion de validacion (filas > 1 y columnas > 2)
(define (validacion n1 n2)
  (cond ((and (>= n1 3) (>= n2 3) (<= n1 10) (<= n2 10)) #t)
        (else #f)))
  
;Funcion llamada para dibujar la matriz
(define (TTT columnas filas)
  (cond ((equal? #t (validacion columnas filas)) (set-vals columnas filas) (send toplevel show #t) (set! matriz-juego (genMatrix columnas filas '())))
        (else
         "Debe introducir valores validos para la matriz")))

;Funcion para convertir de binario a decimal
(define (bin-dec n)
  (cond ((zero? n) n)
        (else
      (+ (modulo n 10) (* 2 (bin-dec (quotient n 10)))))))

;Funcion para borrar del gui los botones
(define (delete-children object (id #f))
  (send object change-children (λ (x)
                  (if id
                      (filter (λ (widget)
                         (not (eq? (send widget ___get-guiml-name) id)))
                          x)
                      '()))))

;Funciones para cambiar de color el label del boton
(define text-size-dc
  (new bitmap-dc% [bitmap (make-object bitmap% 1 1)]))

(define colorable-button%
  (class button%
    (init [(internal-label label)]
          [(initial-color color) "black"]
          [(internal-font font) normal-control-font])
    (define label internal-label)
    (define font internal-font)
    (super-new [label (make-label label font initial-color)]
               [font font])
    (define/override (set-label l)
      (set! label l)
      (super set-label l))
    (define/private (make-label label font color)
      (cond
        [(string? label)
         (match-define-values (w h _ _)
           (send text-size-dc get-text-extent label font))
         (define new-label (make-object bitmap% (exact-ceiling 90) (exact-ceiling 90)))
         (define dc (new bitmap-dc% [bitmap new-label]))
         (send dc set-font font)
         (send dc set-text-foreground color)
         (send dc draw-text label 17 5)
         new-label]
        [else label]))
    (define/public (set-color c)
      (define new-label (make-label label font c))
      (super set-label new-label))))