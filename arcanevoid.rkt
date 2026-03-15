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

(define score 0)
(define hiscore 0)

(define width  800)
(define height 600)

(define bx 100)
(define by 100)
(define vx0 +0.5)
(define vy0 -10.0)
(define vx 0)                           ; exact for detecting that ball "sits"
(define vy 0)
(define px 400)
(define py 560)
(define pvx 0.0)
(define g 1e-1)                         ; gravity rate
(define acc-left-cool-down  0)
(define acc-right-cool-down 0)
(define acc-cool-down-ticks 10)

(define (ball-sits?)
  (= 0 vx vy))

(define (place-ball-on-paddle!)
  (set! bx px)
  (set! by (- py 5))
  (set! vx 0)
  (set! vy 0))

(place-ball-on-paddle!)

(define (launch-ball!)
  (set! vx (+ pvx vx0))
  (set! vy vy0))

(define (acc-paddle! a)
  (set! pvx (+ pvx a)))

(define (handle! ev)
  (match ev
    [(quit-event) (quit!)]
    [(key-event 'down 'escape _ _ _) (quit!)]
    [(key-event 'down 'left _ _ _)
     (acc-paddle! -1)
     (set! acc-left-cool-down acc-cool-down-ticks)]
    [(key-event 'down 'right _ _ _)
     (acc-paddle! +1)
     (set! acc-right-cool-down acc-cool-down-ticks)]
    [(key-event 'down 'space _ _ _)
     (when (ball-sits?)
       (launch-ball!))]
    [_ (void)]))

(define (render-paddle! ren x y)
  (define (cool-down-alpha x)
    (- 1.0 (/ (- acc-cool-down-ticks x)
              acc-cool-down-ticks)))
  (set-draw-color! ren 255 255 255)
  (fill-rect! ren (- x 30) (- y 5) 60 10)
  (when (positive? acc-left-cool-down)
    (set-draw-color-float! ren 1.0 1.0 1.0 (cool-down-alpha acc-left-cool-down))
    (fill-rect! ren (+ x 32) (- y 3) 6 6))
  (when (positive? acc-right-cool-down)
    (set-draw-color-float! ren 1.0 1.0 1.0 (cool-down-alpha acc-right-cool-down))
    (fill-rect! ren (- x 38) (- y 3) 6 6)))

(define (render-ball! ren x y)
  (set-draw-color! ren 200 0 0)
  (fill-rect! ren (- x 5) (- y 5) 10 10))

(define (render-score! ren)
  (set-draw-color! ren 255 255 255)
  (render-debug-text! ren 0 0 (format "High Score: ~s" hiscore))
  (render-debug-text! ren 0 12 (format "Score: ~s" score)))

(define (render-params! ren)
  (set-draw-color! ren 255 255 255)
  (render-debug-text! ren 0 30 (format "vx: ~s" vx))
  (render-debug-text! ren 0 42 (format "vy: ~s" vy))
  (render-debug-text! ren 0 56 (format "pvx: ~s" pvx)))

(define (render! ren)
  (set-draw-color! ren 0 0 0)
  (render-clear! ren)
  (render-paddle! ren px py)
  (render-ball! ren bx by)
  (render-score! ren)
  (render-params! ren))

(define (move-ball!)
  (set! bx (+ bx vx))
  (set! by (+ by vy))
  (unless (ball-sits?)
    (set! vy (+ vy g)))
  (cond
    [(not (< 0 bx width))
     (set! vx (- vx))]
    
    [(or (< by 0)
         (and (< (- px 30) bx (+ px 30))
              (< (- py 10) by (+ py))))
     (unless (or (ball-sits?) (< by 0))
       (set! score (add1 score)))
     (set! vy (- vy))
     (unless (ball-sits?)
       (set! vx (+ vx pvx)))]

    [(< height by)
     (set! hiscore (max hiscore score))
     (set! score 0)
     (place-ball-on-paddle!)]))

(define (move-paddle!)
  (set! px (+ px pvx))
  (when (ball-sits?)
      (set! bx (+ bx pvx)))
  (cond
    [(not (< 30 px (- width 30)))
     (set! px (max 30 (min px (- width 30))))
     (set! pvx (- (/ pvx 2)))]))

(define (cool-down-acc!)
  (when (positive? acc-left-cool-down)
    (set! acc-left-cool-down (sub1 acc-left-cool-down)))
  (when (positive? acc-right-cool-down)
    (set! acc-right-cool-down (sub1 acc-right-cool-down))))

(define (move!)
  (move-ball!)
  (move-paddle!)
  (cool-down-acc!))

(define (game)
  (with-sdl
    (with-window+renderer "Arcane Void" width height (win ren)
      (set-blend-mode! ren 'blend)
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
