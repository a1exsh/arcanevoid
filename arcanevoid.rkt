;;
;; Arcane Void: an Arkanoid clone using Racket and SDL
;; Copyright (C) 2026  Alex Shulgin
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
#lang racket                            ; /base
; TODO: 'require' racket/match

(require sdl3)

(define width 800)
(define height 600)

(define bx 100)
(define by 100)
(define vx 5)
(define vy 5)
(define px 400)
(define py 560)
(define dx 0)                           ; last paddle movement dx

(define (reset!)
  (set! bx 100)
  (set! by 100)
  (set! vx 5)
  (set! vy 5))

(reset!)

(define (ball-sits?)
  (= 0 vx vy))

(define (launch-ball!)
  (set! vx 5)
  (set! vy -5))

(define (move-paddle! x)
  (set! dx (- x px))
  (set! px x)
  (if (ball-sits?)
      (set! bx (+ bx dx))
      #f))

(define (handle! ev)
  (match ev
    [(quit-event) #t]
    [(key-event 'down 'escape _ _ _) #t]
    [(mouse-motion-event x _ _ _ _)
     (move-paddle! x)
     #f]
    [(mouse-button-event 'down 'left _ _ _)
     (if (ball-sits?)
         (launch-ball!)
         #f)
     #f]
    [_ #f]))

(define (render-paddle! ren x y)
  (set-draw-color! ren 255 255 255)
  (fill-rect! ren (- x 30) (- y 5) 60 10))

(define (render-ball! ren x y)
  (set-draw-color! ren 200 0 0)
  (fill-rect! ren (- x 5) (- y 5) 10 10))

(define (render! ren)
  (set-draw-color! ren 0 0 0)
  (render-clear! ren)
  (render-paddle! ren px py)
  (render-ball! ren bx by))

(define (move-ball!)
  (set! bx (+ bx vx))
  (set! by (+ by vy))
  (cond
    [(not (< 0 bx width))
     (set! vx (- vx))]
    
    [(or (< by 0)
         (and (< (- px 30) bx (+ px 30))
              (< (- py 10) by (+ py))))
     (set! vy (- vy))
     (set! vx (+ vx (/ dx 2)))]

    [(< height by)
     (set! bx px)
     (set! by (- py 5))
     (set! vx 0)
     (set! vy 0)]))

(define (move!)
  (if (ball-sits?)
      #f
      (move-ball!))

  ; reset dx after each move so it doesn't stick
  (set! dx 0))

(define (game)
  (with-sdl
    (with-window+renderer "Arcane Void" width height (win ren)
      (let loop ()
        (define quit?
          (for/or ([ev (in-events)])
            (handle! ev)))
        (move!)
        (unless quit?
          (render! ren)
          (render-present! ren)
          (delay! 16)
          (loop))))))

(if (equal? '#("-f") (current-command-line-arguments))
    (game)
    (thread game))
