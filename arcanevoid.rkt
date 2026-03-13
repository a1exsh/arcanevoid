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

(define run? #t)

(define (run!)
  (set! run? #t))

(define (quit!)
  (set! run? #f))

(define width 800)
(define height 600)

(define bx 100)
(define by 100)
(define vx0 10)
(define vy0 -10)
(define vx 0)
(define vy 0)
(define px 400)
(define py 560)
(define pvx 0)
(define dx 0)                           ; last paddle movement dx
(define g 1e-1)                         ; gravity rate

(define (ball-sits?)
  (= 0 vx vy))

(define (place-ball-on-paddle!)
  (set! bx px)
  (set! by (- py 5))
  (set! vx 0)
  (set! vy 0))

(place-ball-on-paddle!)

(define (launch-ball!)
  (set! vx vx0)
  (set! vy vy0))

(define (acc-paddle! a)
  (set! pvx (+ pvx a)))

(define (handle! ev)
  (match ev
    [(quit-event) (quit!)]
    [(key-event 'down 'escape _ _ _) (quit!)]
    [(key-event 'down 'left _ _ _) (acc-paddle! -1)]
    [(key-event 'down 'right _ _ _) (acc-paddle! +1)]
    ;; [(mouse-motion-event x _ _ _ _)
    ;;  (move-paddle! x)
    ;;  #f]
    [(key-event 'down 'space _ _ _)
     (when (ball-sits?)
       (launch-ball!))]
    ;; [(mouse-button-event 'down 'left _ _ _)
    ;;  ]
    [_ #f;;(println ev)
     ]))

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
  (when (not (ball-sits?))
    (set! vy (+ vy g)))
  (cond
    [(not (< 0 bx width))
     (set! vx (- vx))]
    
    [(or (< by 0)
         (and (< (- px 30) bx (+ px 30))
              (< (- py 10) by (+ py))))
     (set! vy (- vy))
     (set! vx (+ vx (/ dx 2)))]

    [(< height by)
     (place-ball-on-paddle!)]))

(define (move-paddle!)
  ;; (set! dx (- x px))
  (set! px (+ px pvx))
  (when (ball-sits?)
      (set! bx (+ bx pvx)))
  (cond
    [(not (< 30 px (- width 30)))
     (set! px (max 30 (min px (- width 30))))
     (set! pvx (- (/ pvx 2)))]))

(define (move!)
  (move-ball!)
  (move-paddle!)

  ; reset dx after each move so it doesn't stick
  (set! dx 0))

(define (game)
  (with-sdl
    (with-window+renderer "Arcane Void" width height (win ren)
      (let loop ()
        (for/or ([ev (in-events)])
          (handle! ev))
        (move!)
        (when run?
          (render! ren)
          (render-present! ren)
          (delay! 16)
          (loop))))))

(run!)
(if (equal? '#("-f") (current-command-line-arguments))
    (game)
    (thread game))
