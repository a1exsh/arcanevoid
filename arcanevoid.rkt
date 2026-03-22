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
#lang racket/base

(require racket/match
         sdl3)

(define run? #t)

(define (run!)
  (set! run? #t))

(define (quit!)
  (set! run? #f))

(define pause? #f)

(define (toggle-pause!)
  (set! pause? (not pause?)))

(define score 0)
(define hiscore 0)

;; a cyclic buffer with the current frame number modulo length as the pointer
;; the bigger the buffer, the more accurate the average is
(define last-rendered-frames-ticks (make-vector 200))
(define current-frame-number 0)

(define (record-frame!)
  (vector-set! last-rendered-frames-ticks
               (modulo current-frame-number
                       (vector-length last-rendered-frames-ticks))
               (current-ticks))
  (set! current-frame-number (add1 current-frame-number)))

(define (fps)
  (define n (vector-length last-rendered-frames-ticks))

  (define earliest-frame-ticks
    (vector-ref last-rendered-frames-ticks (modulo current-frame-number n)))

  (define latest-frame-ticks
    (vector-ref last-rendered-frames-ticks (modulo (sub1 current-frame-number) n)))

  (define avg-ticks-per-frame
    (/ (- latest-frame-ticks earliest-frame-ticks)
       n))

  (if (= 0 avg-ticks-per-frame)
      0.0
      (/ 1000.0 avg-ticks-per-frame)))

(define width  800)
(define height 600)

(define bx 100)                         ; ball: x and y position
(define by 100)
(define vx0 +0.5)                       ; ball's initial velocity
(define vy0 -10.0)
(define vx 0)                           ; ball's velocity
(define vy 0)                           ; exact for detecting that ball "sits"
(define px 400)                         ; paddle's position
(define py 560)
(define pvx 0.0)                        ; paddle's horizontal velocity
(define g 1e-1)                         ; gravity rate
(define a 1e-1)                         ; acceleratin kick of the paddle
(define collision-factor 1.00)          ; velocity left after a collision
(define acc-left-cool-down  0)
(define acc-right-cool-down 0)
(define acc-cool-down-ticks 10)
(define speed 1.0)                      ; game speed

(define (speed-up!)
  (set! speed (+ speed 0.1)))

(define (slow-down!)
  (set! speed (max 0.1 (- speed 0.1))))

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

(define (handle! ev)
  (match ev
    [(quit-event) (quit!)]
    [(key-event 'down 'escape _ _ _) (quit!)]
    [(key-event 'down 'p _ _ _) (toggle-pause!)]
    [(key-event 'down 'left-bracket _ _ _) (slow-down!)]
    [(key-event 'down 'right-bracket _ _ _) (speed-up!)]
    [(key-event 'down 'space _ _ _)
     (unless pause?
       (when (ball-sits?)
         (launch-ball!)))]
    [_ (void)]))

(define (poll!)
  (cond
    [(key-pressed? 'left)
     (set! acc-left-cool-down acc-cool-down-ticks)]
    [(key-pressed? 'right)
     (set! acc-right-cool-down acc-cool-down-ticks)]))

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
  (define (round-score sc)
    (/ (round (* 100 sc)) 100.0))
  (set-draw-color! ren 255 255 255)
  (render-debug-text! ren 0 0 (format "High Score: ~s" (round-score hiscore)))
  (render-debug-text! ren 0 12 (format "Score: ~s" (round-score score))))

(define (render-params! ren)
  (set-draw-color! ren 255 255 255)
  (render-debug-text! ren 0 30 (format "vx: ~s" vx))
  (render-debug-text! ren 0 42 (format "vy: ~s" vy))
  (render-debug-text! ren 0 56 (format "pvx: ~s" pvx)))

(define (render-fps! ren)
  (define text (format "FPS: ~s" (inexact->exact (round (fps)))))
  (define text-length (string-length text))
  (define text-width (* 8 text-length))
  (set-draw-color! ren 255 255 255)
  (render-debug-text! ren (- width text-width) 0 text))

(define (render-game-speed! ren)
  (define rounded-speed (/ (round (* 10 speed)) 10.0))
  (define text (format "speed: ~s" rounded-speed))
  (define text-length (string-length text))
  (define text-width (* 8 text-length))
  (set-draw-color! ren 255 255 255)
  (render-debug-text! ren (- width text-width) 12 text))

(define (render-paused! ren)
  (define text "-- PAUSED --")
  (define text-length (string-length text))
  (define text-width (* 8 text-length))
  (define text-height 8)
  (set-draw-color! ren 255 255 255)
  (render-debug-text! ren
                      (/ (- width text-width) 2)
                      (/ (- height text-height) 2)
                      text))

(define (render! ren)
  (set-draw-color! ren 0 0 0)
  (render-clear! ren)
  (render-paddle! ren px py)
  (render-ball! ren bx by)
  (render-score! ren)
  (render-params! ren)
  (render-fps! ren)
  (render-game-speed! ren)
  (when pause?
    (render-paused! ren)))

(define (move-ball-step! n dx dy)
  (set! bx (+ bx dx))
  (set! by (+ by dy))
  (cond
    [(not (< 0 bx width))
     (set! bx (max 0 (min bx (sub1 width))))
     (set! vx (* collision-factor (- vx)))
     (values (- dx) dy)]

    [(or (< by 0)
         (and (< (- px 30) bx (+ px 30))
              (< (- py 10) by (+ py))))
     (if (< by 0)
         (set! by 0)
         (begin
           (set! by (- py 10))
           (set! score (+ score speed))))

     (set! vx (+ vx pvx))
     (set! vy (* collision-factor (- vy)))
     (values (+ dx (/ pvx n))
             (- dy))]

    [(< height by)
     (set! hiscore (max hiscore score))
     (set! score 0)
     (place-ball-on-paddle!)
     (values 0 0)]

    [else (values dx dy)]))

(define (move-ball!)
  (define svx (* speed vx))
  (define svy (* speed vy))
  (define msvxy (max (abs svx) (abs svy)))
  (define n (ceiling (/ msvxy 5.0)))
  (define sdx (/ svx n))
  (define sdy (/ svy n))

  (let loop ([i n] [dx sdx] [dy sdy])
    (unless (or (= i 0)
                (= 0 dx dy))
      (let-values ([(dx dy) (move-ball-step! n dx dy)])
        (loop (sub1 i) dx dy))))

  (unless (ball-sits?)
    ;; apply gravity
    (set! vy (+ vy (* speed g)))))

(define (move-paddle!)
  (define spvx (* speed pvx))
  (define (acc-paddle! acc)
    (set! pvx (+ pvx (* speed acc))))

  (set! px (+ px spvx))
  (when (ball-sits?)
    (set! bx (+ bx spvx)))

  ;; check collision against window edges
  (unless (< 30 px (- width 30))
    (set! px (max 30 (min px (- width 30))))
    (set! pvx (- (/ pvx 2))))

  ;; apply thrusters acceleration
  (when (positive? acc-left-cool-down)
    (when (= acc-left-cool-down acc-cool-down-ticks)
      (acc-paddle! (- a)))
    (set! acc-left-cool-down (sub1 acc-left-cool-down)))

  (when (positive? acc-right-cool-down)
    (when (= acc-right-cool-down acc-cool-down-ticks)
      (acc-paddle! a))
    (set! acc-right-cool-down (sub1 acc-right-cool-down))))

(define (move!)
  (unless (ball-sits?)
    (move-ball!))
  (move-paddle!))

(define (game)
  (with-sdl
    (with-window+renderer "Arcane Void" width height (win ren)
      (set-render-vsync! ren 1)
      (set-blend-mode! ren 'blend)
      (let loop ()
        (for/or ([ev (in-events)])
          (handle! ev))
        (unless pause?
          (poll!)
          (move!))
        (when run?
          (render! ren)
          (render-present! ren)
          ;; (delay! 16)                   ; when not using vsync
          (record-frame!)
          (loop))))))

(run!)
(if (equal? '#("-f") (current-command-line-arguments))
    (game)
    (thread game))
