;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname walls) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

; (check-location "05" "walls.rkt")

(require "extras.rkt")
(require rackunit)
(define TIME-ON-TASK 20)

(provide INITIAL-WORLD)
(provide next-world)
(provide key-handler)
(provide mouse-handler)
(provide end?)
(provide get-balls)
(provide mk-ball)
(provide replace-balls)
(provide get-ball-x)
(provide get-ball-y)
(provide score)
(provide level)
; ======================================== CONSTANTS ===================================

(define WIDTH 400)
(define HEIGHT 400)
(define RADIUS 20)
(define BALL-DIAMETER (* 2 RADIUS))
(define BALL-VELOCITY 10)
(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define MINOR-AXIS 4)
(define INITIAL-WALL-LENGTH 2)
(define FONT-SIZE 10)
(define FONT-COLOR "black")
(define TEXT-POSITION-X 30)
(define TEXT-POSITION-Y 20)
(define WALL-MODE "solid")
(define WALL-COLOR "brown")
(define HIGHEST-SCORE 90)
(define CANVAS-SIZE (* WIDTH HEIGHT))

(define END-GAME-FONT-SIZE 50)
(define END-GAME-IMAGE (text "GAME OVER" END-GAME-FONT-SIZE FONT-COLOR))
(define SCORE-IMAGE (text "SCORE: " FONT-SIZE FONT-COLOR))
(define GOAL-IMAGE (text "GOAL: " FONT-SIZE FONT-COLOR))
(define LEVEL-IMAGE (text "LEVEL: " FONT-SIZE FONT-COLOR))
(define BALL-IMAGE (circle RADIUS "solid" "blue"))
(define WALL-GROW-VELOCITY 16)
(define WALL-GROW-VELOCITY-ONE-SIDE 8)

; ======================================== DATA DEFINITION ==============================

; A ListOf<X> is one of:
; - empty
; - (cons X ListOf<X>)

; TEMPLATE:
; list-fn : ListOf<X> -> ???
(define (list-fn lst)
  (cond
    [(empty? lst) ...]
    [else (... (X-fn (first lst)) ...
               (list-fn (rest lst)) ...)]))

; A Ball is (make-ball Real Real Integer Integer)
;  WHERE 
;   - x-pos y-pos are the x, y-coordinate of the ball, 
;     representing the current position of the ball
;   - vx vy are the velocity of the ball vertically and horizontally. 
(define-struct ball (x-pos y-pos vx vy))

; Balls is a Listof<Ball>, one of
;   - emtpy
;   - (cons Ball ListofBalls)
#;(define (fn-balls bals)
    (cond
      [(empty? bals)...]
      [(cons? bals) (fn-ball (first bals)...)
                    (fn-balls (rest bals)...)]))

; The Orientation is one of
;      - "vertical"
;      - "horizontal"

#; (define (fn-orientation orient)
     (cond
       [(string=? orient "vertical")...]
       [(string=? orient "horizontal")...]))

; A Wall is (make-wall Real Real Real String Boolean)
; WHERE
;  - x is the x coordinate of the wall 
;  - y is the y coordinate of the wall
;  - length is a real number, represneting the current length of the wall
;  - orientation is one of
;      - "vertical"
;      - "horizontal"
;  - active is a Boolean, true if it is active, false otherwise
(define-struct wall (x-pos y-pos length orientation active))

#; (define (fn-wall wal)
     (... (wall-x-pos wal)...
          (wall-y-pos wal)...
          (wall-length wal)...
          (fn-orientation (wall-orientation wal)...)
          (wall-active wal)...)...)

; Walls is a Listof<Wall> is one of 
;  - empty
;  - (cons Wall ListofWalls)

#;(define (fn-walls wals)
    (cond
      [(empty? wals) ...]
      [(cons? wals) (fn-wall (first wals)..)
                    (fn-walls (rest wals)..)]))

; A Shade is a structure (make-shade Posn Posn)
; WHERE 
;  - The first posn is the upleft part of the shade rectangle
;  - The second posn is the downright posn of rectangle
(define-struct shade (upleft downright))

; A ListofShade is one of 
;  - '()
;  - (cons shade ListofShade)
; 
; A World is (make-world ListofBalls ListofWalls String Integer Shades)
; WHERE
;  - A ListofBalls is a list of balls in the world
;  - Listofwalls are the walls built in this current level
;  - Orient which indicate the next wall to build 
;    should be horizontal or vertical
;  - Level is the current level, can be [1, 10]
;  - Shades is a structure, defining the place 
;    where ball can actively move and walls can be built 
;    and based on this the score can be calculated

(define-struct world (balls walls orient level shades))

#; (define (fn-world wld)
     (... (fn-balls (world-balls wld))...
          (fn-walls (world-walls wld))...
          (fn-orientation (world-orient wld))...
          (world-level wld)...
          (fn-shades (world-shades wld))...))

; ======================================== DRAWING ===================================
; render: World -> Image
; RETURN an image given a world where has balls walls level and score
; Structural decomposition on the World
(define (render wld)
  (render-balls 
   (world-balls wld)
   (render-walls 
    (world-walls wld)
    (render-text 
     wld
     (render-shades 
      (world-shades wld)
      BACKGROUND)))))

; TEST
(begin-for-test
  (render 
   (make-world
    (list (make-ball 224 224 -1 -6))
    (list (make-wall 339 200 400 "vertical" false))
    "vertical"
    1
    (list
     (make-shade (make-posn 339 0) (make-posn 400 400)))))
  (render-balls 
   (list (make-ball 224 224 -1 -6))
   (render-walls
    (list (make-wall 339 200 400 "vertical" false))
    (render-text
     (make-world
      (list (make-ball 224 224 -1 -6))
      (list (make-wall 339 200 400 "vertical" false))
      "vertical"
      1
      (list
       (make-shade (make-posn 339 0) (make-posn 400 400))))
     (render-shades 
      (list
       (make-shade (make-posn 339 0) (make-posn 400 400)))
      BACKGROUND)))))


; ====== render-shades =======
; render-shades: Shades Image -> Image
; RETURN an Image with the given list of shades placed on given image
; fUNCTIONAL cOMPOSITION

(define (render-shades shds img)
  (foldr render-each-shade img shds))
;TEST
(begin-for-test
  (check-equal? 
   (render-shades 
    (list
     (make-shade (make-posn 314 0) (make-posn 370 400)))
    BACKGROUND)
   (place-image (rectangle 56 400 "solid" "yellow")
                342 200
                BACKGROUND)))


; render-each-shade: Shade Image -> Image
; RETURN an image where the shade is place on the given image
; fUNCTIONAL cOMPOSITION
(define (render-each-shade shd img)
  (place-image (shade-image shd)
               (shade-x-pos shd)
               (shade-y-pos shd)
               img))

; shade-image: Shade -> Image
; RETURN an image of the shade based on its upleft and down right 
; positions
; Structural decomposition on Shade
(define (shade-image shd)
  (rectangle (get-width (shade-downright shd)
                        (shade-upleft shd))
             (get-height (shade-downright shd)
                         (shade-upleft shd))
             "solid" "yellow"))

; get-width: Posn Posn -> Real
; RETURN the width of the rectangle framed by two posn
; Structural decomposition 
(define (get-width dr ul)
  (- (posn-x dr)
     (posn-x ul)))

; get-height: Posn Posn -> Real
; RETURN the height of the rectangle framed by two posn
; Structural decomposition 
(define (get-height dr ul)
  (- (posn-y dr)
     (posn-y ul)))

; shade-x-pos: Shade -> Real
; RETURN the central-x position
; ; Structural decomposition 
(define (shade-x-pos shd)
  (get-central-x (shade-upleft shd) (shade-downright shd)))

; get-central-x: Posn Posn -> Real
; RETURN the central x coordinate of the two Posns
; Structural decomposition 
(define (get-central-x ul dr)
  (/ (+ (posn-x ul)
        (posn-x dr)) 2))

; shade-y-pos: Shade -> Real
; RETURN the central-y position
; Structural decomposition  
(define (shade-y-pos shd)
  (get-central-y (shade-upleft shd) (shade-downright shd)))

; get-central-y: POSN Posn -> Real
; RETRUN the central y corrdinate
; Structural decomposition 
(define (get-central-y ul dr)
  (/ (+ (posn-y ul)
        (posn-y dr)) 2))

; ====== render-balls =======

; render-balls: ListofBalls Image -> Image
; RETURN an Image with the given list of balls placed on given image
; Structural decomposition on the Ball
(define (render-balls loballs img)
  (foldr render-each-ball img loballs))

; TEST
(begin-for-test
  (check-equal?
   (render-balls 
    (list (make-ball 205 224 5 -6)) BACKGROUND)
   (place-image BALL-IMAGE
                205 224
                BACKGROUND)))

;
; render-each-ball: Ball Image -> Image
; RETURN an image with the given ball placed on the given image
; functional composition
(define (render-each-ball bal img)
  (place-image BALL-IMAGE
               (ball-x-pos bal)
               (ball-y-pos bal)
               img))

; ====== render-walls =======

; render-walls: Listof<Wall>  Image -> Image
; RETURN a new image with the list of walls placed on the given image
; Structural decomposition on the List of Walls
(define (render-walls lowalls img)
  (foldl render-each-wall img lowalls))

; TEST
(begin-for-test
  (check-equal?
   (render-walls 
    (list (make-wall 250 200 400 "vertical" false))
    BACKGROUND)
   (place-image (wall-image
                 (make-wall 250 200 400 "vertical" false))
                250 200
                BACKGROUND)))
; render-each-wall: Wall Image -> Image
; RETURN a new image by placing the wall onto the given image
; Structural decomposition
(define (render-each-wall wal img)
  (place-image (wall-image wal)
               (wall-x-pos wal)
               (wall-y-pos wal)
               img))


; wall-image: Wall -> Image
; Return the rendered wall's image
; Structural decomposition
(define (wall-image wal)
  (if (string=? (wall-orientation wal) "vertical")
      (render-vertical wal)
      (render-horizontal wal)))
; TEST
(begin-for-test
  (check-equal?
   (wall-image 
    (make-wall 250 200 400 "vertical" false))
   (render-vertical (make-wall 250 200 400 "vertical" false))))

; render-vertical: Wall -> Image
; RETURN an image of teh vertical wall
; Structural decomposition
(define (render-vertical wal)
  (rectangle MINOR-AXIS (wall-length wal) WALL-MODE WALL-COLOR))

; TEST
(begin-for-test
  (check-equal?
   (render-vertical 
    (make-wall 250 200 400 "vertical" false))
   (rectangle MINOR-AXIS 400 "solid" WALL-COLOR)))

; render-horizontal: wall -> Image
; RETURN an image of teh horizontal wall
; Structural decomposition
(define (render-horizontal wal)
  (rectangle (wall-length wal) MINOR-AXIS WALL-MODE WALL-COLOR))

; TEST
(begin-for-test
  (check-equal?
   (render-horizontal 
    (make-wall 250 200 400 "horizontal" false))
   (rectangle 400 MINOR-AXIS "solid" WALL-COLOR)))
; ====== render-texts =======

; render-text: World Image -> Image
; RETURN a new image where the level info and score
; info displayed on the given background image
; STRATEGY: 
(define (render-text wld img)
  (display-level-score (world-level wld)
                       (score wld)
                       img))

; display-level-score: Integer Integer Image -> Image
; RETURN a new image where the level and score displayed
; Functional Composition
(define (display-level-score leve scor img)
  (display-level leve
                 (display-score scor
                                (display-goal (goal-score leve)
                                              img))))
; display-level: Integer Image -> Image
; RETURN an image where the level is added to the given image
; Functional Composition
(define (display-level leve img)
  (place-image 
   (beside LEVEL-IMAGE 
           (text (number->string leve) FONT-SIZE FONT-COLOR))
   TEXT-POSITION-X
   TEXT-POSITION-Y
   img))

; display-score: Integer Image -> Image
; RETURN an image where the score is added to the given image
; Functional Composition
(define (display-score scor img)
  (place-image 
   (beside SCORE-IMAGE
           (text (number->string scor) FONT-SIZE FONT-COLOR))
   (- WIDTH TEXT-POSITION-X)
   TEXT-POSITION-Y
   img))

; display-goal: Integer Image -> Image
; RETURN an image where the goal score is added to the given image
; Functional Composition
(define (display-goal goal img)
  (place-image
   (beside GOAL-IMAGE
           (text (number->string goal) FONT-SIZE FONT-COLOR))
   (- WIDTH TEXT-POSITION-X)
   (* 2 TEXT-POSITION-Y)
   img))

; ====== render-last =======

; render-last: World -> Image
; Given a World
; RETURN an image of the last world with END GAME displayed
; Functional Composition
(define (render-last wld)
  (place-image END-GAME-IMAGE
               (/ WIDTH 2)
               (/ HEIGHT 2)
               (render wld)))

(begin-for-test
  (check-equal? 
   (render-last 
    (make-world
     (list (make-ball 240 110 8 6))
     (list
      (make-wall 220 104 34 "vertical" true))
     "vertical"
     1
     '()))
   (place-image
    END-GAME-IMAGE
    (/ WIDTH 2)
    (/ HEIGHT 2)
    (render
     (make-world
      (list (make-ball 240 110 8 6))
      (list
       (make-wall 220 104 34 "vertical" true))
      "vertical"
      1
      '())))))



; ======================================== RANDOM BALL ================================

; random-balls: Integer -> Balls
; RETURN a list of Ball with a given number
; Structural Decomposition
(define (random-balls num )
  (cond
    [(= 0 num) empty]
    [(< 0 num) 
     (cons (make-ball (+ RADIUS (random (* 2 (- WIDTH RADIUS))))
                      (+ RADIUS (random (* 2 (- HEIGHT RADIUS)))) 
                      (+ 1 (random  BALL-VELOCITY))
                      (+ 1 (random  BALL-VELOCITY)))
           (random-balls (sub1 num)))]))

; INITIAL WORLD
(define INITIAL-BALLS (random-balls 1))
(define INITIAL-ORIENT "vertical")
(define INITIAL-LEVEL 1)

(define INITIAL-WORLD 
  (make-world INITIAL-BALLS '() INITIAL-ORIENT INITIAL-LEVEL '()))

; ======================================== INTERACTION ================================

; next-world: World -> World
; RETURN a new world after one tick
; WHERE
;  - World will upgrade to the next level where the 
;    level's goal score is achieved; 
;     - one more ball added at random speed
;     - Walls built disappears
;     - level displayed will increment
;     - Shades will disappear
;  - if goal is not achieved, keep bouncing
;    and creating walls & shades
; Functional Composition
(define (next-world wld)
  (if (get-goal-score wld)
      (next-level-world wld)
      (continue-game wld)))

; TEST
(begin-for-test
  (next-world 
   (make-world
    (list
     (make-ball 340 119 1 -1)
     (make-ball 325 80 -10 -10))
    (list (make-wall 99 200 400 "vertical" false))
    "vertical"
    2
    (list
     (make-shade (make-posn 0 0) (make-posn 99 400)))))
  (make-world 
   (list (make-ball 341 118 1 -1)
         (make-ball 315 70 -10 -10))
   (list (make-wall 99 200 400 "vertical" false))
   "vertical"
   2
   (list
    (make-shade (make-posn 0 0) (make-posn 99 400)))))
(check-random 
 (next-world 
  (make-world
   (list
    (make-ball 340 119 1 -1)
    (make-ball 325 80 -10 -10))
   (list (make-wall 99 200 400 "vertical" false)
         (make-wall 300 200 400 "vertical" false))
   "vertical"
   2
   (list
    (make-shade (make-posn 0 0) (make-posn 99 400))
    (make-shade (make-posn 99 0) (make-posn 360 400)))))
 (make-world 
  (random-balls 3)
  '()
  "vertical"
  3
  '()))


; get-goal-score: World -> Boolean
; RETURN true of the world reaches the goal score
; Strucutral decomposition
(define (get-goal-score wld)
  (reach-goal-score? (score wld)
                     (world-level wld)))

; reach-goal-score?: Integer Integer -> Boolean
; GIVEN a Score and a Level, 
; RETURN true if the score reached the goal score
; Functional Composition
(define (reach-goal-score? scor leve)
  (>= scor (goal-score leve)))

; goal-score: Integer -> Integer
; GIVEN the level
; RETURN the goal score required for that level
; Functional Composition
(define (goal-score leve)
  (if (< HIGHEST-SCORE (+ 50 (* 5 leve)))
      HIGHEST-SCORE
      (+ 50 (* 5 leve))))

; TEST
(begin-for-test
  (check-equal? 
   (goal-score 3)
   65)
  (check-equal?
   (goal-score 9)
   90))

; next-level-world: World -> World
; RETURN a new World where there is one more ball 
; and they are having random position with random 
; velocity
; Structural Decomposition
(define (next-level-world wld)
  (make-world (random-balls (+ (world-level wld) 1))
              '()
              "vertical"
              (+ (world-level wld) 1)
              '()))

; ====== CONTINUE GAME ========

; continue-game: World  -> World
; RETURN A new World after one tick
; WHERE
;  - If wall is an active wall,check if it should be deactive;
;    if it is not yet reach the walls, keep growing; otherwise
;    mark as de-active (active as false)
;  - Ball bounces if it hits the wall, otherwise keep moving
; Functional Composition

(define (continue-game wld)
  (if (active-wall? wld)
      (balls-move (check-and-grow wld))
      (balls-move wld)))

; TEST
(begin-for-test
  (check-equal?
   (continue-game 
    (make-world
     (list (make-ball 134 260 6 8))
     (list (make-wall 215 190 380 "vertical" true))
     "vertical"
     1
     '()))
   (make-world
    (list (make-ball 140 268 6 8))
    (list (make-wall 215 194 388 "vertical" true))
    "vertical"
    1
    '()))
  (check-equal?
   (continue-game 
    (make-world
     (list (make-ball 134 260 6 8))
     '()
     "vertical"
     1
     '()))
   (make-world
    (list (make-ball 140 268 6 8))
    '()
    "vertical"
    1
    '())))

; active-wall?: World -> Boolean
; RETURN true if there is active wall
; false otherwise
; Structural Decomposition
(define (active-wall? wld)
  (active? (world-walls wld)))

; active? : Walls -> Boolean
; return true if the first wall is still active
; Structural Decomposition
(define (active? wals)
  (cond
    [(empty? wals) false]
    [(cons? wals) (get-wall-status (first wals))]))

; get-wall-status: Wall -> Boolean
; RETURN true if the wall is active
; false otherwise
; Structural Decomposition
(define (get-wall-status wal)
  (if (wall? wal)
      (wall-active wal)
      false))

; get-last-wall: World -> Wall
; RETURN a latest wall given a non-empty walls
; Structural decomposition
(define (get-last-wall wld)
  (get-wall (world-walls wld)))

; TEST
(begin-for-test
  (check-equal? 
   (get-last-wall 
    (make-world
     (list (make-ball 134 260 6 8))
     (list (make-wall 215 190 380 "vertical" true))
     "vertical"
     1
     '()))
   (make-wall 215 190 380 "vertical" true)))

; get-wall: Walls -> Wall
; RETURN the first Wall if Walls are not empty
; Structural decomposition
(define (get-wall wals)
  (first wals))

; ====== Check-and-grow =======
; check-and-grow: World -> World
; RETURN a world where the walls in World will grow
; given it is active, 
; - if its two ends have reached two sides
;   (by checking if both ends of the wall meet the existing walls
;   or the canvas), de-active the wall, and generate/not generate shade 
; - if not hit both sides, keep growing on both sides/one side

; Structural decomposition
(define (check-and-grow wld)
  (if (both-ends-meet-walls? (world-walls wld))
      (deactive-wall-check-shade wld) 
      (wall-grow wld)))

; TEST
(begin-for-test
  (check-equal? 
   (check-and-grow 
    (make-world
     (list (make-ball 281 201 9 8))
     (list
      (make-wall 176 81 200 "horizontal" true)
      (make-wall 325 200 400 "vertical" false))
     "horizontal"
     1
     (list (make-shade (make-posn 325 0)
                       (make-posn 400 400)))))
   (make-world 
    (list (make-ball 281 201 9 8))
    (list
     (make-wall 176 81 216 "horizontal" true)
     (make-wall 325 200 400 "vertical" false))
    "horizontal"
    1
    (list (make-shade (make-posn 325 0)
                      (make-posn 400 400)))))
  (check-equal? 
   (check-and-grow 
    (make-world
     (list (make-ball 281 201 9 8))
     (list (make-wall 325 200 400 "vertical" true))
     "horizontal"
     1
     '()))
   (make-world 
    (list (make-ball 281 201 9 8))
    (list (make-wall 325 200 400 "vertical" false))
    "horizontal"
    1
    (list (make-shade (make-posn 325 0)
                      (make-posn 400 400))))))

; both-ends-meet-walls?: Walls -> Boolean
; RETURN true if next tick will make both ends of the active wall
; meet the existing wall or the canvas
; Structural Decomposition
(define (both-ends-meet-walls? wals)
  (check-ends-meet-walls (grow-freely (first wals))
                         (rest wals)))

; check-wall-meet-walls: Wall Walls -> Boolean
; RETURN true if the wall both sides hit walls
; Structural Decomposition
(define (check-ends-meet-walls wal wals)
  (if (horizontal? wal)
      (horizontal-check wal wals)
      (vertical-check wal wals)))

; horizontal-check: Wall ListofWall -> Boolean      
; RETURN true of the wall in active is hitting both ends of the wall
; Functional Composition
(define (horizontal-check wal wals)
  (horizontal-check-walls 
   (- (wall-x-pos wal) (/ (wall-length wal) 2))
   (+ (wall-x-pos wal) (/ (wall-length wal) 2)) wals))

; TEST
(begin-for-test
  (check-equal? 
   (horizontal-check (make-wall 20 50 42 "horizontal" true)
                     (list (make-wall 40 80 160 "vertical" false)))
   true)
  (check-equal? 
   (horizontal-check (make-wall 250 150 500 "horizontal" true) '())
   true))

; horizontal-check-walls: Real Real Walls -> Boolean
; RETURN true if the wall is hiting both sides given the two 
; ends and a list of wall
; Structural Decomposition
(define (horizontal-check-walls small large wals)
  (and (hor-small-end-hit-walls small wals)
       (hor-large-end-hit-walls large wals)))

; hor-small-end-hit-walls: Real Walls -> Boolean
; RETURN true of the horizontal wall's small end hit one of the walls
; Structural Decomposition
(define (hor-small-end-hit-walls small wals)
  (cond
    [(empty? wals) (<= small 0)]
    [(cons? wals) (or (hor-small-end-hit small (first wals))
                      (hor-small-end-hit-walls small (rest wals)))]))

; hor-large-end-hit-walls: Real Walls -> Boolean
; RETURN true if the horizontal wall' large end hits one of the walls
; Structural Decomposition
(define (hor-large-end-hit-walls large wals)
  (cond
    [(empty? wals)(>= large WIDTH)]
    [(cons? wals) (or (hor-large-end-hit large (first wals))
                      (hor-large-end-hit-walls large (rest wals)))]))

; hor-small-end-hit: Real Wall -> Boolean
; RETURN true if the smaller side is smaller than the vertical wall
; Functional Composition
(define (hor-small-end-hit small wal)
  (if (horizontal? wal)
      false
      (<= (- small WALL-GROW-VELOCITY-ONE-SIDE) 
          (get-minor wal) 
          (+ small WALL-GROW-VELOCITY-ONE-SIDE))))

; TEST
(begin-for-test
  (check-equal? 
   (hor-small-end-hit 
    20 (make-wall 20 200 400 "vertical" false))
   true)
  (check-equal? 
   (hor-small-end-hit 
    20 (make-wall 20 200 400 "horizontal" false))
   false))


; hor-large-end-hit: Real Wall -> Boolean
; RETURN true if the LARGER side is growing larger than the vertical wall
; Functional Composition
(define (hor-large-end-hit large wal)
  (if (horizontal? wal)
      false
      (<= (- large WALL-GROW-VELOCITY) 
          (get-minor wal) 
          (+ large WALL-GROW-VELOCITY-ONE-SIDE))))

; get-minor: Wall -> Real
; RETURN the minor wall
; Structural Decomposition
(define (get-minor wal)
  (if (horizontal? wal)
      (wall-y-pos wal)
      (wall-x-pos wal)))

; vertical-check: Wall ListofWall -> Boolean      
; RETURN true of the vertical wall in active is hitting both ends of the wall
; Functional Composition
(define (vertical-check wal wals)
  (vertical-check-walls 
   (small-end-wall wal)
   (large-end-wall wal) wals))

; TEST
(begin-for-test
  (check-equal? 
   (vertical-check (make-wall 50 50 40 "vertical" true)
                   (list (make-wall 100 70 200 "horizontal" false)))
   false)
  (check-equal? 
   (vertical-check (make-wall 50 50 100 "vertical" true)
                   (list (make-wall 100 100 200 "horizontal" false)))
   true))

; vertical-check-walls: Real Real Walls -> Boolean
; RETURN true if the two sides both hit the walls or canvas
; Structural Decomposition
(define (vertical-check-walls small large wals)
  (and (ver-small-end-hit-walls small wals)
       (ver-large-end-hit-walls large wals)))

; ver-small-end-hit-walls: Real Walls -> Boolean
; Structural Decomposition
(define (ver-small-end-hit-walls small wals)
  (cond
    [(empty? wals) (<= small 0)]
    [(cons? wals) (or (ver-small-end-hit small (first wals))
                      (ver-small-end-hit-walls small (rest wals)))]))

; ver-large-end-hit-walls: Real Walls -> Boolean
; Structural Decomposition
(define (ver-large-end-hit-walls large wals)
  (cond
    [(empty? wals)(>= large WIDTH)]
    [(cons? wals) (or (ver-large-end-hit large (first wals))
                      (ver-large-end-hit-walls large (rest wals)))]))

; ver-small-end-hit: Real Wall -> Boolean
; RETURN true if the smaller side is smaller than the vertical wall
; Functional Composition
(define (ver-small-end-hit small wal)
  (if (horizontal? wal)
      (<= (- small WALL-GROW-VELOCITY-ONE-SIDE) 
          (get-minor wal) 
          (+ small WALL-GROW-VELOCITY-ONE-SIDE))
      false))

; ver-large-end-hit: Real Wall -> Boolean
; RETURN true if the LARGER side is growing larger than the horizontal wall
; Functional Composition
(define (ver-large-end-hit large wal)
  (if (horizontal? wal)
      (<= (- large WALL-GROW-VELOCITY-ONE-SIDE) 
          (get-minor wal) 
          (+ large WALL-GROW-VELOCITY-ONE-SIDE))
      false))


; ============= deactive-wall-check-shade ==============

; deactive-wall-check-shade: World -> World
; RETURN a new Walls where the wall has hit both sides of the
; borders and needs to be deactivated and also check if the shades
; need to increase
; Structural decomposition
(define (deactive-wall-check-shade wld)
  (make-world (world-balls wld)
              (deactivate-wall (world-walls wld))
              (world-orient wld)
              (world-level wld)
              (update-shades 
               (world-with-updated-wall wld))))

; TEST
(begin-for-test
  (check-equal? 
   (deactive-wall-check-shade 
    (make-world
     (list (make-ball 299 140 -10 10))
     (list (make-wall 20 200 402 "vertical" true))
     "vertical"
     1
     empty))
   (make-world
    (list (make-ball 299 140 -10 10))
    (list (make-wall 20 200 400 "vertical" false))
    "vertical"
    1
    (list (make-shade (make-posn 0 0)
                      (make-posn 20 400))))
   "there is a ball on the right side of the wall and the 
    wall is hitting both ends of the canvas")
  (check-equal? 
   (deactive-wall-check-shade 
    (make-world
     (list (make-ball 299 140 -10 10))
     (list (make-wall 20 210 382 "vertical" true)
           (make-wall 200 20 400 "horizontal" false))
     "vertical"
     1
     (list (make-shade (make-posn 0 0)
                       (make-posn 400 20)))))
   (make-world
    (list (make-ball 299 140 -10 10))
    (list (make-wall 20 210 380 "vertical" false)
          (make-wall 200 20 400 "horizontal" false))
    "vertical"
    1
    (list (make-shade (make-posn 0 20)
                      (make-posn 20 400))
          (make-shade (make-posn 0 0)
                      (make-posn 400 20))))
   "with a horizontal wall already built and both sides of the new wall
    has already touched the canvas/existing wall, generating new shade and 
    deactive the corresponding area"))


; world-with-updated-wall: World -> World
; RETURN an updated world with the wall's length and status
; updated so as to make a better shade
; Structural decomposition on World

(define (world-with-updated-wall wld)
  (make-world (world-balls wld)
              (deactivate-wall (world-walls wld))
              (world-orient wld)
              (world-level wld)
              (world-shades wld)))

; TEST
(begin-for-test
  (check-equal? 
   (world-with-updated-wall
    (make-world
     (list (make-ball 299 140 -10 10))
     (list (make-wall 20 210 382 "vertical" true)
           (make-wall 200 20 400 "horizontal" false))
     "vertical"
     1
     (list (make-shade (make-posn 0 0)
                       (make-posn 400 20)))))
   (make-world
    (list (make-ball 299 140 -10 10))
    (list (make-wall 20 210 380 "vertical" false)
          (make-wall 200 20 400 "horizontal" false))
    "vertical"
    1
    (list (make-shade (make-posn 0 0)
                      (make-posn 400 20))))))

; deactivate-wall: Walls -> Walls
; RETURN deactived walls GIVEN the current walls with 
; the lastest built being active
; Structural decomposition
(define (deactivate-wall wals)
  (cons (change-status (first wals) (rest wals))
        (rest wals)))

; TEST
(begin-for-test
  (deactivate-wall 
   (list (make-wall 20 210 382 "vertical" true)
         (make-wall 200 20 400 "horizontal" false)))
  (list (make-wall 20 210 380 "vertical" false)
        (make-wall 200 20 400 "horizontal" false)))

; change-status: Wall Walls- > Wall
; RETURN a wall with the active field false, and updated
; x-pos and y-pos to be strictly flushing at the existing walls
; Structural decomposition
(define (change-status wal wals)
  (if (horizontal? wal)
      (hor-change-status wal wals)
      (ver-change-status wal wals)))
; TEST
(begin-for-test
  (check-equal? 
   (change-status (make-wall 20 210 382 "vertical" true)
                  (list (make-wall 200 20 400 "horizontal" false)))
   (make-wall 20 210 380 "vertical" false)))

; hor-change-status: Wall Walls -> Wall
; RETURN a new horizontal wall 
; Structural Decomposition
(define (hor-change-status wal wals)
  (make-wall (hor-update-x-pos wal wals)
             (wall-y-pos wal)
             (hor-update-length wal wals)
             (wall-orientation wal)
             false))

; TEST
(begin-for-test
  (hor-change-status 
   (make-wall 210 20 385 "horizontal" true)
   (list (make-wall 20 200 400 "vertical" false)))
  (make-wall 210 20 380 "horizontal" false))

; hor-update-x-pos: Wall Walls -> Real
; return the updated x-pos of the wall
; Functional Compostion
(define (hor-update-x-pos wal wals)
  (/ (+ (hor-update-small-end wal wals)
        (hor-update-large-end wal wals)) 2))

; hor-update-length: Wall Walls -> Real
; RETURN the new length of the Wall
; Functional Compostion
(define (hor-update-length wal wals)
  (- (hor-update-large-end wal wals)
     (hor-update-small-end wal wals)))

; hor-update-large-end: Wall Walls -> Real
; RETURN the new large end coordinate of the wall
; Structural Decomposition
(define (hor-update-large-end wal wals)
  (cond
    [(empty? wals) WIDTH]
    [(cons? wals) (hor-update-large-end-hit wal wals)]))

; hor-update-large-end-hit: Wall Walls -> Real
; RETURN the UPDATED new large end coordinate of the wall
; Structural Decomposition

(define (hor-update-large-end-hit wal wals)
  (if (hor-large-end-hit (large-end-wall wal) 
                         (first wals))
      (get-minor (first wals))
      (hor-update-large-end wal (rest wals))))

; TEST
(begin-for-test
  (check-equal? 
   (hor-update-large-end-hit
    (make-wall 210 20 385 "horizontal" true)
    (list (make-wall 20 200 400 "vertical" false)))
   400)
  (check-equal? 
   (hor-update-large-end-hit
    (make-wall 190 20 385 "horizontal" true)
    (list (make-wall 380 200 400 "vertical" false)))
   380))

; hor-update-small-end: Wall Walls -> Real
; Structural Decomposition
(define (hor-update-small-end wal wals)
  (cond
    [(empty? wals) 0]
    [(cons? wals)(if (hor-small-end-hit (small-end-wall wal) 
                                        (first wals))
                     (get-minor (first wals))
                     (hor-update-small-end wal (rest wals)))]))

; ====== vertical-change-status ======

; ver-change-status: Wall Walls -> Wall
; RETURN a new horizontal wall 
; Structural Decomposition
(define (ver-change-status wal wals)
  (make-wall (wall-x-pos wal)
             (ver-update-y-pos wal wals)
             (ver-update-length wal wals)
             (wall-orientation wal)
             false))

;  TEST
(begin-for-test
  (check-equal? 
   (ver-change-status (make-wall 20 210 385 "vertical" true)
                      (list (make-wall 200 20 400 "horizontal" false)))
   (make-wall 20 210 380 "vertical" false)))

; ver-update-y-pos: Wall Walls -> Real
;; Functional Compostion
(define (ver-update-y-pos wal wals)
  (/ (+ (ver-update-small-end wal wals)
        (ver-update-large-end wal wals)) 2))

(begin-for-test
  (check-equal? 
   (ver-update-y-pos (make-wall 20 210 385 "vertical" true)
                     (list (make-wall 200 20 400 "horizontal" false)))
   210))

; ver-update-length: Wall Walls -> Real
; Functional Compostion
(define (ver-update-length wal wals)
  (- (ver-update-large-end wal wals)
     (ver-update-small-end wal wals)))
; test
(begin-for-test
  (check-equal? 
   (ver-update-y-pos (make-wall 20 210 385 "vertical" true)
                     (list (make-wall 200 20 400 "horizontal" false)))
   210))

; ver-update-large-end: Wall Walls -> Real
;; Structural Decomposition
(define (ver-update-large-end wal wals)
  (cond
    [(empty? wals) WIDTH]
    [(cons? wals) (if (ver-large-end-hit (large-end-wall wal) 
                                         (first wals))
                      (get-minor (first wals))
                      (ver-update-large-end wal (rest wals)))]))

(begin-for-test
  (check-equal? 
   (ver-update-large-end
    (make-wall 20 210 385 "vertical" true)
    (list (make-wall 200 20 400 "horizontal" false)))
   400))

; ver-update-small-end: Wall Walls -> Real
; Structural Decomposition
(define (ver-update-small-end wal wals)
  (cond
    [(empty? wals) 0]
    [(cons? wals)(if (ver-small-end-hit (small-end-wall wal) 
                                        (first wals))
                     (get-minor (first wals))
                     (ver-update-small-end wal (rest wals)))]))

(begin-for-test
  (check-equal? 
   (ver-update-small-end
    (make-wall 20 210 385 "vertical" true)
    (list (make-wall 200 20 400 "horizontal" false)))
   20))

; ==================== UPDATE-SHADES =====================

; update-shades : World -> Shades
; GIVEN a World where a wall just finish building
; RETURN a New world where the Shades are updated
; Data Decomposition
(define (update-shades wld)
  (update-shade-ball-wall (world-balls wld)
                          (world-walls wld)
                          (world-shades wld)))

; TESTS
(begin-for-test
  (check-equal?
   (update-shades 
    (make-world
     (list (make-ball 150 335 -3 -3))
     (list (make-wall 308 200 400 "vertical" false))
     "vertical"
     1
     '()))
   (list (make-shade (make-posn 308 0)
                     (make-posn 400 400)))))

; update-shade-ball-wall: Balls Walls Shades -> Shades
; RETURN a new shades GIVEN A listofballs, a non-empty list of walls
; and a lish of original shades
; Data Decomposition
(define (update-shade-ball-wall bals wals shds)
  (check-new-shade bals (first wals)(rest wals) shds))

; TESTS
(begin-for-test
  (check-equal?
   (update-shade-ball-wall 
    (list (make-ball 150 335 -3 -3))
    (list (make-wall 308 200 400 "vertical" false))
    '())
   (list (make-shade (make-posn 308 0)
                     (make-posn 400 400)))))

; check-new-shade: Balls Wall Walls Shades -> Shades
; RETURN the new list of shade with the new one generated by 
; the newly built wall added to the list
; Data Decomposition
(define (check-new-shade bals wal wals shds)
  (if (horizontal? wal)
      (horizontal-shade-ball-wall bals wal wals shds)
      (vertical-shade-ball-wall bals wal wals shds)))



; ================ listofwalls - update shades =================
; === horizontal ===
; horizontal-shade-ball-wall: Balls Wall Walls Shades -> Shades
; RETURN THE new shades after updating
; Functional Composition
(define (horizontal-shade-ball-wall bals wal wals shds)
  (if (balls-in-hor-up-fake-shade bals wal wals)
      (check-bellow-fake-shade bals wal wals shds)
      (add-up-fake-shade wal wals shds)))

; TEST
(begin-for-test
  (check-equal? 
   (horizontal-shade-ball-wall 
    (list (make-ball 30 50 3 9))
    (make-wall 200 150 400 "horizontal" true)
    '()
    '())
   (list (make-shade 
          (make-posn 0 150)
          (make-posn 400 400))))
  (check-equal? 
   (horizontal-shade-ball-wall 
    (list (make-ball 330 250 3 9))
    (make-wall 200 150 400 "horizontal" true)
    '()
    '())
   (list (make-shade 
          (make-posn 0 0)
          (make-posn 400 150))))
  (check-equal? 
   (horizontal-shade-ball-wall 
    (list (make-ball 330 250 3 9)
          (make-ball 30 50 4 6))
    (make-wall 200 150 400 "horizontal" true)
    '()
    '())
   '())
  (check-equal? 
   (horizontal-shade-ball-wall 
    (list (make-ball 330 250 3 9))
    (make-wall 200 150 400 "horizontal" true)
    (list (make-wall 200 50 400 "horizontal" false))
    (list (make-shade 
           (make-posn 0 0) (make-posn 400 50))))
   (list (make-shade 
          (make-posn 0 50)(make-posn 400 150))
         (make-shade 
          (make-posn 0 0) (make-posn 400 50))))
  (check-equal? 
   (horizontal-shade-ball-wall 
    (list (make-ball 330 250 3 9))
    (make-wall 200 150 400 "horizontal" true)
    (list (make-wall 200 350 400 "horizontal" false))
    (list (make-shade 
           (make-posn 0 350) (make-posn 400 400))))
   (list (make-shade 
          (make-posn 0 0)(make-posn 400 150))
         (make-shade 
          (make-posn 0 350) (make-posn 400 400)))))

; check-bellow-fake-shade: balls Wall Walls Shades -> Shades
; RETURN a list of shades with the updated bellow shade attached/not
; Functional Composition
(define (check-bellow-fake-shade bals wal wals shds)
  (if (balls-in-hor-lower-fake-shade bals wal wals)
      shds
      (add-bellow-fake-shade wal wals shds)))

; add-up-fake-shade: Wall Walls Shades -> Shades
; RETURN a new list of shade with the new up shade added
; Functional Composition
(define (add-up-fake-shade wal wals shds)
  (cons (get-up-fake-shade wal wals) shds))

; add-bellow-fake-shade: Wall Walls Shades -> Shades
; RETURN a new list of shade with the new shade added
; Functional Composition
(define (add-bellow-fake-shade wal wals shds)
  (cons (get-lower-fake-shade wal wals) shds))

; balls-in-hor-up-fake-shade: Balls Wall Walls -> Boolean
; true if there is a ball in the upper fake shade
; false otherwise
; Data decomposition on the Ball
(define (balls-in-hor-up-fake-shade bals wal wals)
  (cond
    [(empty? bals) false]
    [(cons? bals) (or (ball-in-shade? (first bals)
                                      (get-up-fake-shade wal wals))
                      (balls-in-hor-up-fake-shade (rest bals) wal wals))]))

; ball-in-shade? : Ball Shade -> Boolean
; RETURN true if the ball is in the shade
(define (ball-in-shade? bal shd)
  (ball-in-posn-shade? (shade-upleft shd) bal (shade-downright shd)))

; ball-in-posn-shade? : Posn Ball Posn -> Boolean
; TRUE if the ball is in side the area featured by two posn
(define (ball-in-posn-shade? up bal down)
  (and (< (posn-x up) (ball-x-pos bal) (posn-x down))
       (< (posn-y up) (ball-y-pos bal) (posn-y down))))

; balls-in-hor-lower-fake-shade: Balls Wall Walls -> Boolean
; true if there is a ball in the lower fake shade
; false otherwise
; Data decomposition on the Ball
(define (balls-in-hor-lower-fake-shade bals wal wals)
  (cond
    [(empty? bals) false]
    [(cons? bals) (or (ball-in-shade? (first bals)
                                      (get-lower-fake-shade wal wals))
                      (balls-in-hor-lower-fake-shade (rest bals) wal wals))]))

; get-up-fake-shade: Wall Walls -> Shade
; return SHADE (fake) above the newly generated line
; Functional Composition
(define (get-up-fake-shade wal wals)
  (get-up-shade-from-given-walls 
   wal (get-closest-upper-wal wal wals)))

; get-lower-fake-shade: Wall Walls -> Shade
; return SHADE (fake) bellow the newly generated line
; Functional Composition
(define (get-lower-fake-shade wal wals)
  (get-lower-shade-from-given-walls 
   wal (get-closest-lower-wal wal wals)))

; get-up-shade-from-given-walls: Wall Wall -> Shade
; RETURN a shade given two walls with one main wall
; Data decomposition on the Wall
(define (get-up-shade-from-given-walls walmain wal)
  (make-shade (make-posn (small-end-wall walmain) (wall-y-pos wal))
              (make-posn (large-end-wall walmain)(wall-y-pos walmain))))

; get-lower-shade-from-given-walls: Wall Wall -> Shade
; RETURN a shade given two walls with one main wall
; Data decomposition on the Wall
(define (get-lower-shade-from-given-walls walmain wal)
  (make-shade (make-posn (small-end-wall walmain) 
                         (wall-y-pos walmain))
              (make-posn (large-end-wall walmain) 
                         (wall-y-pos wal))))

; get-closest-upper-wal: Wall Walls -> Wall
; RETURN the upper wall that is closest to given wall
; Data decomposition on the Wall
(define (get-closest-upper-wal wal wals)
  (cond
    [(empty? wals)(make-wall 200 0 WIDTH "horizontal" false)]
    [(cons? wals)(first (get-unempty-hor-up-sorted-wall wal wals))]))

; get-unempty-hor-up-sorted-wall: Wall Walls -> Walls
; RETURN an unempty list so that the higher fn can decompose the first
; Functional composition
(define (get-unempty-hor-up-sorted-wall wal wals)
  (if (empty? (get-hor-up-sorted-wall wal wals))
      (list (make-wall 200 0 WIDTH "horizontal" false))
      (get-hor-up-sorted-wall wal wals)))

; get-hor-up-sorted-wall: Wall Walls -> Walls
; RETURN the horizontal walls that are sorted all on up side with
; closest one the first
; Functional Compositiion
(define (get-hor-up-sorted-wall wal wals)
  (hor-sort-walls 
   wal (filter-walls-on-up-side 
        wal (filter-walls-same-ori wal wals))))



; get-closest-lower-wal: Wall Walls -> Wall
; RETURN the lower wall that is closest to given wall
; Data decomposition on the Wall
(define (get-closest-lower-wal wal wals)
  (cond
    [(empty? wals)(make-wall (/ WIDTH 2) HEIGHT WIDTH "horizontal" false)]
    [(cons? wals)(first (hor-sort-walls 
                         wal (filter-walls-on-down-side 
                              wal (filter-walls-same-ori wal wals))))]))

; TEST
(begin-for-test
  (check-equal?
   (get-closest-lower-wal 
    (make-wall 200 70 400 "horizontal" false)
    (list 
     (make-wall (/ WIDTH 2) HEIGHT WIDTH "horizontal" false)))
   (make-wall (/ WIDTH 2) HEIGHT WIDTH "horizontal" false))
  (check-equal?
   (get-closest-lower-wal 
    (make-wall 200 70 400 "horizontal" false)
    (list 
     (make-wall 200 40 400 "horizontal" false)
     (make-wall 200 80 400 "horizontal" false)
     (make-wall 200 100 400 "horizontal" false)))
   (make-wall 200 80 400 "horizontal" false)))


; filter-walls-same-ori: Wall Walls -> Walls
; Return all the walls with the same orientation
; Functional Composition
(define (filter-walls-same-ori wal wals)
  (local (; same-ori : Wall -> Boolean
          (define (same-ori wa)
            (if (horizontal? wal)
                (horizontal? wa)
                (not (horizontal? wa)))))
    (filter same-ori wals)))

; filter-walls-on-up-side: Wall Walls -> Walls
; Return a list of walls that are on up side of the wall
; GIVEN ALL the walls are horizontal 
; Functional Composition  
(define (filter-walls-on-up-side wal wals)
  (local (; up-side: Wall -> Boolean
          (define (up-side wa)
            (< (wall-y-pos wa) 
               (wall-y-pos wal))))
    (filter up-side wals)))

; filter-walls-on-down-side: Wall Walls -> Walls
; Return a list of walls that are on down side of(bellow)the wall
; GIVEN ALL the walls are horizontal 
; Functional Composition
(define (filter-walls-on-down-side wal wals)
  (local (; down-side: Wall -> Boolean
          (define (down-side wa)
            (> (wall-y-pos wa) 
               (wall-y-pos wal))))
    (filter down-side wals)))

; TEST
(begin-for-test
  (check-equal?
   (filter-walls-on-down-side
    (make-wall 200 50 400 "horizontal" false)
    (list
     (make-wall 200 20 400 "horizontal" false)
     (make-wall 200 60 400 "horizontal" false)
     (make-wall 200 120 400 "horizontal" false)))
   (list
    (make-wall 200 60 400 "horizontal" false)
    (make-wall 200 120 400 "horizontal" false))))

; hor-sor-walls: Wall Walls -> Walls
; Return a list of walls in order, where
; the wall closest to the given wall will come first
; Functional Composition
(define (hor-sort-walls wal wals)
  (local (; compt: Wall Wall -> Boolean
          (define (compt wa1 wa2)
            (< (abs (- (wall-y-pos wa1)
                       (wall-y-pos wal)))
               (abs (- (wall-y-pos wa2)
                       (wall-y-pos wal))))))
    (sort wals compt)))

(begin-for-test
  (check-equal?
   (hor-sort-walls
    (make-wall 200 50 400 "horizontal" false)
    (list
     (make-wall 200 90 400 "horizontal" false)
     (make-wall 200 60 400 "horizontal" false)
     (make-wall 200 120 400 "horizontal" false)))
   (list
    (make-wall 200 60 400 "horizontal" false)
    (make-wall 200 90 400 "horizontal" false)
    (make-wall 200 120 400 "horizontal" false))))


; === vertical ===
; vertical-shade-ball-wall: Balls Wall Walls Shades -> Shades
; RETURN THE new shades after updating
; Functional Composition
(define (vertical-shade-ball-wall bals wal wals shds)
  (if (balls-in-ver-left-fake-shade bals wal wals)
      (check-right-fake-shade bals wal wals shds)
      (add-left-fake-shade wal wals shds)))
; TEST
(begin-for-test
  (check-equal? 
   (vertical-shade-ball-wall 
    (list (make-ball 50 30 3 9))
    (make-wall 200 200 400 "vertical" true)
    '()
    '())
   (list (make-shade 
          (make-posn 200 0)
          (make-posn 400 400)))
   "balls on left side of the wall")
  (check-equal? 
   (vertical-shade-ball-wall 
    (list (make-ball 330 250 3 9))
    (make-wall 200 200 400 "vertical" true)
    '()
    '())
   (list (make-shade 
          (make-posn 0 0)
          (make-posn 200 400)))
   "balls on one side of the wall")
  (check-equal? 
   (vertical-shade-ball-wall 
    (list (make-ball 330 250 3 9)
          (make-ball 30 50 4 6))
    (make-wall 200 200 400 "vertical" true)
    '()
    '())
   '()
   "balls on both side of the wall")
  (check-equal? 
   (vertical-shade-ball-wall 
    (list (make-ball 330 250 3 9))
    (make-wall 200 200 400 "vertical" true)
    (list (make-wall 50 200 400 "vertical" false))
    (list (make-shade 
           (make-posn 0 0) (make-posn 50 400))))
   (list (make-shade 
          (make-posn 50 0)(make-posn 200 400))
         (make-shade 
          (make-posn 0 0) (make-posn 50 400)))
   "balls on one side and there is another wall on another side")
  (check-equal? 
   (vertical-shade-ball-wall 
    (list (make-ball 330 250 3 9))
    (make-wall 200 200 400 "vertical" true)
    (list (make-wall 350 200 400 "vertical" false))
    (list (make-shade 
           (make-posn 350 0) (make-posn 400 400))))
   (list (make-shade 
          (make-posn 0 0)(make-posn 200 400))
         (make-shade 
          (make-posn 350 0) (make-posn 400 400)))
   "balls on one side and there is another wall on same side"))


; check-right-fake-shade: balls Wall Walls Shades -> Shades
; RETURN a list of shades with the updated right shade attached/not
; Functional Composition
(define (check-right-fake-shade bals wal wals shds)
  (if (balls-in-ver-right-fake-shade bals wal wals)
      shds
      (add-right-fake-shade wal wals shds)))

(begin-for-test
  (check-equal? 
   (check-right-fake-shade 
    (list (make-ball 30 50 3 4))
    (make-wall 200 200 400 "vertical" false)
    '()
    '())
   (list (make-shade (make-posn 200 0)
                     (make-posn 400 400)))))

; add-left-fake-shade: Wall Walls Shades -> Shades
; RETURN a new list of shade with the new left shade added
; Functional Composition
(define (add-left-fake-shade wal wals shds)
  (cons (get-left-fake-shade wal wals) shds))

; add-right-fake-shade: Wall Walls Shades -> Shades
; RETURN a new list of shade with the new shade added
; Functional Composition
(define (add-right-fake-shade wal wals shds)
  (cons (get-right-fake-shade wal wals) shds))

(begin-for-test
  (check-equal? 
   (add-right-fake-shade 
    (make-wall 200 200 400 "vertical" false)
    '()
    '())
   (list (make-shade (make-posn 200 0)
                     (make-posn 400 400)))))

; balls-in-ver-left-fake-shade: Balls Wall Walls -> Boolean
; true if there is a ball in the left fake shade
; false otherwise
; Data decomposition on the Ball
(define (balls-in-ver-left-fake-shade bals wal wals)
  (cond
    [(empty? bals) false]
    [(cons? bals) (or (ball-in-shade? (first bals)
                                      (get-left-fake-shade wal wals))
                      (balls-in-ver-left-fake-shade (rest bals) wal wals))]))

; balls-in-ver-right-fake-shade: Balls Wall Walls -> Boolean
; true if there is a ball in the lower fake shade
; false otherwise
; Data decomposition on the Ball
(define (balls-in-ver-right-fake-shade bals wal wals)
  (cond
    [(empty? bals) false]
    [(cons? bals) (or (ball-in-shade? (first bals)
                                      (get-right-fake-shade wal wals))
                      (balls-in-ver-right-fake-shade (rest bals) wal wals))]))

; get-left-fake-shade: Wall Walls -> Shade
; return SHADE (fake) on left of the newly generated line
; Functional Composition
(define (get-left-fake-shade wal wals)
  (get-left-shade-from-given-walls 
   wal (get-closest-left-wal wal wals)))

; get-right-fake-shade: Wall Walls -> Shade
; return SHADE (fake) on right of the newly generated line
; Functional Composition
(define (get-right-fake-shade wal wals)
  (get-right-shade-from-given-walls 
   wal (get-closest-right-wal wal wals)))

(begin-for-test
  (check-equal? 
   (get-right-fake-shade 
    (make-wall 200 200 400 "vertical" false)
    '())
   (make-shade (make-posn 200 0)
               (make-posn 400 400))))


; get-left-shade-from-given-walls: Wall Wall -> Shade
; RETURN a shade given two walls with one main wall
; Data decomposition on the Wall
(define (get-left-shade-from-given-walls walmain wal)
  (make-shade (make-posn (wall-x-pos wal) 
                         (small-end-wall walmain))
              (make-posn (wall-x-pos walmain)
                         (large-end-wall walmain))))

; get-right-shade-from-given-walls: Wall Wall -> Shade
; RETURN a shade given two walls with one main wall
; Data decomposition on the Wall
(define (get-right-shade-from-given-walls walmain wal)
  (make-shade (make-posn (wall-x-pos walmain)
                         (small-end-wall walmain))
              (make-posn (wall-x-pos wal)
                         (large-end-wall walmain))))

(begin-for-test
  (check-equal? 
   (get-right-shade-from-given-walls 
    (make-wall 200 200 400 "vertical" false)
    (make-wall 400 200 400 "vertical" false))
   (make-shade (make-posn 200 0)
               (make-posn 400 400))))

; get-closest-left-wal: Wall Walls -> Wall
; RETURN the left wall that is closest to given wall
; Data decomposition on the Wall
(define (get-closest-left-wal wal wals)
  (cond
    [(empty? wals)(make-wall 0 200 WIDTH "vertical" false)]
    [(cons? wals)(first (get-unempty-ver-left-sorted-wall wal wals))]))

; get-unempty-ver-left-sorted-wall: Wall Walls -> Walls
; RETURN an unempty list so that the higher fn can decompose the first
; Functional composition
(define (get-unempty-ver-left-sorted-wall wal wals)
  (if (empty? (get-ver-left-sorted-wall wal wals))
      (list (make-wall 0 200 WIDTH "vertical" false))
      (get-ver-left-sorted-wall wal wals)))

; get-ver-left-sorted-wall: Wall Walls -> Walls
; RETURN the vertical walls that are sorted all on left side with
; closest one the first
; Functional Compositiion
(define (get-ver-left-sorted-wall  wal wals)
  (ver-sort-walls 
   wal (filter-walls-on-left-side 
        wal (filter-walls-same-ori wal wals))))

(begin-for-test
  (check-equal?
   (get-ver-left-sorted-wall 
    (make-wall 200 200 400 "vertical" false)
    (list
     (make-wall 10 200 400 "vertical" false)
     (make-wall 80 200 400 "vertical" false)
     (make-wall 100 200 400 "vertical" false)))
   (list
    (make-wall 100 200 400 "vertical" false)
    (make-wall 80 200 400 "vertical" false)
    (make-wall 10 200 400 "vertical" false))))

; get-closest-right-wal: Wall Walls -> Wall
; RETURN the right wall that is closest to given wall
; Data decomposition on the Wall
(define (get-closest-right-wal wal wals)
  (cond
    [(empty? wals)(make-wall WIDTH 200 WIDTH "vertical" false)]
    [(cons? wals)(first (get-unempty-ver-right-sorted-wall wal wals))]))

; test
(begin-for-test
  (check-equal? 
   (get-closest-right-wal
    (make-wall 200 200 400 "vertical" false)
    '())
   (make-wall 400 200 400 "vertical" false))
  (check-equal? 
   (get-closest-right-wal
    (make-wall 200 200 400 "vertical" false)
    (list 
     (make-wall 300 200 400 "vertical" false)))
   (make-wall 300 200 400 "vertical" false)))

; get-unempty-ver-right-sorted-wall: Wall Walls -> Walls
; RETURN an unempty list so that the higher fn can decompose the first
; Functional composition
(define (get-unempty-ver-right-sorted-wall wal wals)
  (if (empty? (get-ver-right-sorted-wall wal wals))
      (list (make-wall 400 200 WIDTH "vertical" false))
      (get-ver-right-sorted-wall wal wals)))

; get-ver-right-sorted-wall: Wall Walls -> Walls
; RETURN the vertical walls that are sorted all on right side with
; closest one the first
; Functional Compositiion
(define (get-ver-right-sorted-wall  wal wals)
  (ver-sort-walls 
   wal (filter-walls-on-right-side 
        wal (filter-walls-same-ori wal wals))))

(begin-for-test
  (check-equal?
   (get-ver-right-sorted-wall 
    (make-wall 20 200 400 "vertical" false)
    (list
     (make-wall 150 200 400 "vertical" false)
     (make-wall 80 200 400 "vertical" false)
     (make-wall 100 200 400 "vertical" false)))
   (list
    (make-wall 80 200 400 "vertical" false)
    (make-wall 100 200 400 "vertical" false)
    (make-wall 150 200 400 "vertical" false))))

; filter-walls-on-left-side: Wall Walls -> Walls
; Return a list of walls that are on left side of the wall
; GIVEN ALL the walls are vertical
; Functional Composition  
(define (filter-walls-on-left-side wal wals)
  (local (; left-side: Wall -> Boolean
          (define (left-side wa)
            (< (wall-x-pos wa) 
               (wall-x-pos wal))))
    (filter left-side wals)))

; filter-walls-on-right-side: Wall Walls -> Walls
; Return a list of walls that are on right side of(bellow)the wall
; GIVEN ALL the walls are vertical 
; Functional Composition
(define (filter-walls-on-right-side wal wals)
  (local (; right-side: Wall -> Boolean
          (define (right-side wa)
            (> (wall-x-pos wa) 
               (wall-x-pos wal))))
    (filter right-side wals)))

; ver-sor-walls: Wall Walls -> Walls
; Return a list of walls in order, where
; the wall closest to the given wall will come first
; Functional Composition
(define (ver-sort-walls wal wals)
  (local (; compt: Wall Wall -> Boolean
          (define (compt wa1 wa2)
            (< (abs (- (wall-x-pos wa1)
                       (wall-x-pos wal)))
               (abs (- (wall-x-pos wa2)
                       (wall-x-pos wal))))))
    (sort wals compt)))



; ================ wall-grow ==================

; wall-grow: World -> World
; GIVEN A World where the last built Wall is active
; RETURN a new World where the wall grows 8 pixels
; for each side
; Structural Decomposition
(define (wall-grow wld)
  (make-world
   (world-balls wld)
   (grow (world-walls wld))
   (world-orient wld)
   (world-level wld)
   (world-shades wld)))

; TEST
(begin-for-test
  (check-equal? 
   (wall-grow 
    (make-world
     (list (make-ball 50 40 9 10))
     (list
      (make-wall 219 192 8 "horizontal" true)
      (make-wall 172 195 418 "vertical" false))
     "vertical"
     1
     empty))
   (make-world
    (list (make-ball 50 40 9 10))
    (list
     (make-wall 219 192 24 "horizontal" true)
     (make-wall 172 195 418 "vertical" false))
    "vertical"
    1
    empty)))


; grow: Walls -> Walls
; RETURN a list of walls with the first one grow
; Structural Decomposition
(define (grow wals)
  (cons (grow-last-wall (first wals) (rest wals))
        (rest wals)))

; TEST
(begin-for-test
  (check-equal?
   (grow (list (make-wall 219 192 8 "horizontal" true)
               (make-wall 172 195 418 "vertical" false)))
   (list (make-wall 219 192 24 "horizontal" true)
         (make-wall 172 195 418 "vertical" false))))

; grow-last-wall: Wall Walls -> Wall
; GIVEN an active wall and a list of inactive walls
; RETURN a new wall growing
; Functional Composition
(define (grow-last-wall wal wals)
  (if (one-end-meets-walls? wal wals)
      (grow-one-side wal wals)
      (grow-freely wal)))

; one-end-meets-walls?: Wal Walls -> Boolean
; RETURN true if one end of the active wall
; meet the existing wall or the canvas
; Structural Decomposition
(define (one-end-meets-walls? wal wals)
  (if (horizontal? wal)
      (one-horizontal-check wal wals)
      (one-vertical-check wal wals)))

; TEST
(begin-for-test
  (check-equal? 
   (one-end-meets-walls?
    (make-wall 40 50 8 "horizontal" true)
    (list (make-wall 80 100 200 "vertical" false)))
   false))

; one-horizontal-check: Wall ListofWall -> Boolean      
; RETURN true if the wall in active is hitting one end of the wall
; Functional Composition
(define (one-horizontal-check wal wals)
  (one-horizontal-check-walls 
   (small-end-wall wal)
   (large-end-wall wal) wals))

; TEST
(begin-for-test
  (check-equal? 
   (one-horizontal-check (make-wall 40 50 8 "horizontal" true)
                         (list (make-wall 80 100 200 "vertical" false)))
   false)
  (check-equal? 
   (one-horizontal-check (make-wall 50 20 68 "horizontal" true)
                         (list (make-wall 80 100 200 "vertical" false)))
   true))

; one-horizontal-check-walls: Real Real Walls -> Boolean
; RETURN true if the wall is hiting one side given the two 
; ends and a list of wall
; Functional Composition
(define (one-horizontal-check-walls small large wals)
  (or (hor-small-end-hit-walls small wals)
      (hor-large-end-hit-walls large wals)))

; one-vertical-check: Wall ListofWall -> Boolean      
; RETURN true if one of the vertical wall ends in active 
; is hitting one of the walls
; Functional Composition
(define (one-vertical-check wal wals)
  (one-vertical-check-walls 
   (small-end-wall wal)
   (large-end-wall wal) wals))

; TEST
(begin-for-test
  (check-equal? 
   (vertical-check (make-wall 50 50 40 "vertical" true)
                   (list (make-wall 100 70 200 "horizontal" false)))
   false)
  (check-equal? 
   (vertical-check (make-wall 50 50 100 "vertical" true)
                   (list (make-wall 100 100 200 "horizontal" false)))
   true))

; one-vertical-check-walls: Real Real Walls -> Boolean
; RETURN true if one of the two sides of the vertical wall 
; hits the walls or canvas
; Structural Decomposition
(define (one-vertical-check-walls small large wals)
  (or (ver-small-end-hit-walls small wals)
      (ver-large-end-hit-walls large wals)))

; TEST
(begin-for-test
  (check-equal? 
   (one-vertical-check-walls 
    20 70 (list (make-wall 50 70 100 "horizontal" false)))
   true))


; grow-one-side: Wall Walls -> Wall
; RETURN a new Wall where only one side is open
; Functional Composition
(define (grow-one-side wal wals)
  (if (small-end-meet? wal wals)
      (wall-grow-large-end wal)
      (wall-grow-small-end wal)))

; Structural Decomposition
(define (filter-orient-hor wal)
  (string=? "horizontal" (wall-orientation wal)))
; Structural Decomposition
(define (filter-orient-ver wal)
  (string=? "vertical" (wall-orientation wal)))

; small-end-meet? Wall Walls -> Boolean
; RETURN true if the smaller end meets
; Functional Composition
(define (small-end-meet? wal wals)
  (if (horizontal? wal)
      (hor-small-end-hit? wal wals)
      (ver-small-end-hit? wal wals)))

; TEST
(begin-for-test
  (check-equal? 
   (small-end-meet? 
    (make-wall 20 190 380 "vertical" true)
    (list (make-wall 200 390  400 "horizontal" false)))
   true)
  (check-equal? 
   (small-end-meet? 
    (make-wall 20 190 340 "vertical" true)
    (list (make-wall 200 390  400 "horizontal" false)))
   false)
  (check-equal? 
   (small-end-meet? 
    (make-wall 190 20 380 "horizontal" true)
    (list (make-wall 390 200 400 "vertical" false)))
   true)
  (check-equal? 
   (small-end-meet? 
    (make-wall 190 20 340 "horizontal" true)
    (list (make-wall 390 200 400 "vertical" false)))
   false))


; hor-small-end-hit?: Wall  Walls -> Boolean
; RETURN true if the small end hits wall/canvas
; Functional Composition
(define (hor-small-end-hit? wal wals)
  (hor-small-end-hit-walls (small-end-wall wal) 
                           (filter filter-orient-ver wals)))

; ver-small-end-hit?: Wall Walls -> Boolean
; RETURN true if the small end hits wall/canvas
; Functional Composition
(define (ver-small-end-hit? wal wals)
  (ver-small-end-hit-walls (small-end-wall wal)
                           (filter filter-orient-hor wals)))

; small-end-wall: Wall -> Real
; RETURN the vertical wall's short end y coordinate
; Structural decomposition
(define (small-end-wall wal)
  (if (horizontal? wal)
      (- (wall-x-pos wal) (/ (wall-length wal) 2))
      (- (wall-y-pos wal) (/ (wall-length wal) 2))))

; large-end-wall: Wall -> Real
; RETURN the vertical wall's large end y coordinate
; Structural decomposition
(define (large-end-wall wal)
  (if (horizontal? wal)
      (+ (wall-x-pos wal) (/ (wall-length wal) 2))
      (+ (wall-y-pos wal) (/ (wall-length wal) 2))))

; wall-grow-large-end: Wall -> Wall
; RETURN a new wall where the short end keeps the same;
; the large end keeps growing
; Functional Composition
(define (wall-grow-large-end wal)
  (if (horizontal? wal)
      (horizontal-large-end-grow wal)
      (vertical-large-end-grow wal)))

; TEST
(begin-for-test
  (check-equal? 
   (wall-grow-large-end (make-wall 300 300 188 "horizontal" true))
   (make-wall 304 300 196 "horizontal" true)))

; horizontal-large-end-grow: Wall -> Wall
; RETURN a new Wall where the length increase 8 pixel and 
; center moved right 4 pixels
; Structural Decomposition
(define (horizontal-large-end-grow wal)
  (make-wall (+ (wall-x-pos wal) (/ WALL-GROW-VELOCITY-ONE-SIDE 2))
             (wall-y-pos wal)
             (+ (wall-length wal) WALL-GROW-VELOCITY-ONE-SIDE)
             (wall-orientation wal)
             (wall-active wal)))

; vertical-large-end-grow: Wall -> Wall
; RETURN a new Wall where the length increase 8 pixel and 
; center moved down 4 pixels
; Structural Decomposition
(define (vertical-large-end-grow wal)
  (make-wall (wall-x-pos wal)
             (+ (wall-y-pos wal) (/ WALL-GROW-VELOCITY-ONE-SIDE 2))
             (+ (wall-length wal) WALL-GROW-VELOCITY-ONE-SIDE)
             (wall-orientation wal)
             (wall-active wal)))

; wall-grow-small-end: Wall -> Wall
; RETURN a new wall where
;  - length grow by 8pixels
;  - center move accordingly by 4 pixels
; Functional Composition
(define (wall-grow-small-end wal)
  (if (horizontal? wal)
      (horizontal-small-end-grow wal)
      (vertical-small-end-grow wal)))

(begin-for-test
  (check-equal? 
   (wall-grow-small-end 
    (make-wall 20 210 380 "vertical" true))
   (make-wall 20 206 388 "vertical" true))
  (check-equal? 
   (wall-grow-small-end 
    (make-wall 210 20 380 "horizontal" true))
   (make-wall 206 20 388 "horizontal" true)))

; horizontal-small-end-grow: Wall -> Wall
; RETURN a new Wall where the length increase 8 pixel and 
; center moved right 4 pixels
; Structural Decomposition
(define (horizontal-small-end-grow wal)
  (make-wall (- (wall-x-pos wal) (/ WALL-GROW-VELOCITY-ONE-SIDE 2))
             (wall-y-pos wal)
             (+ (wall-length wal) WALL-GROW-VELOCITY-ONE-SIDE)
             (wall-orientation wal)
             (wall-active wal)))

; vertical-small-end-grow: Wall -> Wall
; RETURN a new Wall where the length increase 8 pixel and 
; center moved down 4 pixels
; Structural Decomposition
(define (vertical-small-end-grow wal)
  (make-wall (wall-x-pos wal)
             (- (wall-y-pos wal) (/ WALL-GROW-VELOCITY-ONE-SIDE 2))
             (+ (wall-length wal) WALL-GROW-VELOCITY-ONE-SIDE)
             (wall-orientation wal)
             (wall-active wal)))

; grow-freely: Wall -> Wall
; return a Wall with the length increased
; Structural Decomposition
(define (grow-freely wal)
  (make-wall (wall-x-pos wal)
             (wall-y-pos wal)
             (+ WALL-GROW-VELOCITY (wall-length wal))
             (wall-orientation wal)
             (wall-active wal)))

; balls-move: World -> World
; REUTRN a new World
; WHERE 
;  - if balls hit the wall, ball bounce and move
;  - if balls not hit moves forward
; Functional Composition
(define (balls-move wld)
  (replace-balls wld
                 (balls-move-walls (world-balls wld)
                                   (world-walls wld))))

; balls-move-walls: Balls Walls -> Balls
; GIVEN A balls and walls
; RETURN a updated list of balls 
; Structural Decomposition
(define (balls-move-walls balls walls)
  (cond
    [(empty? balls) empty]
    [(cons? balls) (cons (move-or-bounce (first balls) walls)
                         (balls-move-walls (rest balls) walls))]))

; move-or-bounce: Ball Walls -> Ball
; GIVEN ONE Ball and Walls
; RETURN a new Ball where it may bounce due to hitting walls
; or simply move freely
; Structural Decomposition
(define (move-or-bounce bal wals)
  (cond
    [(empty? wals) (move-in-canvas bal)]
    [(cons? wals) (if (one-ball-hit-wall? (simple-move bal) (first wals))
                      (bounce-ball bal (first wals))
                      (move-or-bounce bal (rest wals)))]))
; TEST
(begin-for-test
  (check-equal? 
   (move-or-bounce (make-ball 32 351 -12 -1) '())
   (make-ball 20 350 -12 -1))
  (check-equal?
   (move-or-bounce (make-ball 20 350 -12 -1) '())
   (make-ball 20 350 12 -1)))

; bounce-ball: Ball Wall -> Ball
; return a new Ball after hitting the wall
; Functional Composition
(define (bounce-ball bal wal)
  (if (horizontal? wal)
      (bounce-horizontal bal wal)
      (bounce-vertical bal wal)))
; TEST
(begin-for-test
  (check-equal? 
   (bounce-ball 
    (make-ball 40 90 2 -4)
    (make-wall (+ RADIUS 40) 200 400 "vertical" false))
   (make-ball 40 90 -2 -4))
  (check-equal? 
   (bounce-ball 
    (make-ball 40 90 -2 -4)
    (make-wall (- 40 RADIUS) 200 400 "vertical" false))
   (make-ball 40 90 2 -4))
  (check-equal? 
   (bounce-ball 
    (make-ball 40 90 2 4)
    (make-wall 200 (+ RADIUS 90) 400 "horizontal" false))
   (make-ball 40 90 2 -4))
  (check-equal? 
   (bounce-ball 
    (make-ball 40 90 2 4)
    (make-wall 200 (+ RADIUS 96) 400 "horizontal" false))
   (make-ball 40 96 2 -4))
  (check-equal? 
   (bounce-ball 
    (make-ball 40 90 2 -4)
    (make-wall 200 (- 88 RADIUS) 400 "horizontal" false))
   (make-ball 40 88 2 4)))

; bounce-horizontal: Ball Wall -> Ball
; RETURN a new ball which hits teh horizontal wall
; Structural Decomposition
(define (bounce-horizontal bal wal)
  (make-ball (ball-x-pos bal)
             (update-ball-y-pos (ball-y-pos bal) wal)
             (ball-vx bal)
             (- 0 (ball-vy bal))))

; update-ball-y-pos: Real Wall -> Real
; RETURN the new y coordinate given teh current y and the wall
; Data deComposition
(define (update-ball-y-pos curr-y wal)
  (if (< curr-y (wall-y-pos wal))
      (- (wall-y-pos wal) RADIUS)
      (+ (wall-y-pos wal) RADIUS)))

; bounce-vertical: Ball Wall -> Ball
; RETURN a ball which hits a vertical wall and bounce
; data decomposition
(define (bounce-vertical bal wal)
  (make-ball (update-ball-x-pos (ball-x-pos bal) wal)
             (ball-y-pos bal)
             (- 0 (ball-vx bal))
             (ball-vy bal)))

; update-ball-x-pos: Real Wall -> Real
; RETURN the updated x coordinate; given the current
; x coordinate adn the wall
; data deComposition on wall
(define (update-ball-x-pos curr-x wal)
  (if (< curr-x (wall-x-pos wal))
      (- (wall-x-pos wal) RADIUS)
      (+ (wall-x-pos wal) RADIUS)))

; move-in-canvas: Ball -> Ball
; RETURN a new ball
; WHERE
;  - BOUNCE if it hits the canvas
;  - move freely if not 
; Functional Composition
(define (move-in-canvas bal)
  (if (next-ball-hit? (simple-move bal))
      (bouncing-ball bal)
      (simple-move bal)))

(begin-for-test 
  (check-equal? 
   (move-in-canvas (make-ball 47 25 -5 -10))
   (make-ball 47 20 -5 10))
  (check-equal? 
   (move-in-canvas (make-ball 20 350 -12 -1))
   (make-ball 20 350 12 -1)))

; next-ball-hit? Ball -> Boolean
; return true of the next ball will hit the edge of 
; the canvas, false otherwise
; Given the next ball
; Functional Composition
(define (next-ball-hit? bal)
  (not
   (and (<= RADIUS (ball-x-pos bal) (- WIDTH RADIUS))
        (<= RADIUS (ball-y-pos bal) (- HEIGHT RADIUS)))))

; bouncing-ball: Ball -> Ball
; GIVEN a Ball where the next world will make it out of canvas
; RETURN a new Ball stop at the wall with velocities changed
; Functional Composition
(define (bouncing-ball bal)
  (if (hit-left-or-right? bal)
      (hit-left-right bal)
      (hit-up-down bal)))

(begin-for-test
  (check-equal? 
   (bouncing-ball (make-ball 47 378 10 5))
   (make-ball 47 380 10 -5))
  (check-equal? 
   (bouncing-ball (make-ball 23 59 -5 10))
   (make-ball 20 59 5 10))
  (check-equal?
   (bouncing-ball (make-ball 47 25 -5 -10))
   (make-ball 47 20 -5 10)))

; hit-left-or-right?: Ball -> Boolean
;
; data deComposition on ball
(define (hit-left-or-right? bal)
  (or (> RADIUS (+ (ball-vx bal)(ball-x-pos bal)))
      (< (- WIDTH RADIUS)(+ (ball-vx bal)(ball-x-pos bal)))))

; hit-left-right: Ball -> Ball
; Functional Composition
(define (hit-left-right bal)
  (if (> RADIUS (+ (ball-vx bal)(ball-x-pos bal)))
      (hit-left bal)
      (hit-right bal)))

; hit-left: Ball -> Ball
; RETURN A NEW ball after hitting the left wall
; data deComposition on Ball
(define (hit-left bal)
  (make-ball RADIUS 
             (ball-y-pos bal)
             (- 0 (ball-vx bal))
             (ball-vy bal)))

; hit-right: Ball -> Ball
; RETURN A NEW ball after hitting the right wall
; data deComposition
(define (hit-right bal)
  (make-ball (- WIDTH RADIUS)
             (ball-y-pos bal)
             (- 0 (ball-vx bal))
             (ball-vy bal)))

; TEST
(begin-for-test
  (hit-right (make-ball 390 90 2 5))
  (make-ball 380 90 -2 5))

; hit-up-down: Ball -> Ball
;; data deComposition
(define (hit-up-down bal)
  (if (> RADIUS (+ (ball-y-pos bal) 
                   (ball-vy bal)))
      (hit-up bal)
      (hit-down bal)))

; hit-up: Ball -> Ball
; data deComposition
(define (hit-up bal)
  (make-ball (ball-x-pos bal)
             RADIUS
             (ball-vx bal)
             (- 0 (ball-vy bal))))

; hit-right: Ball -> Ball
;; data deComposition
(define (hit-down bal)
  (make-ball (ball-x-pos bal)
             (- HEIGHT RADIUS)
             (ball-vx bal)
             (- 0 (ball-vy bal))))


; simple-move: Ball -> Ball
; data deComposition Ball
(define (simple-move bal)
  (make-ball (+ (ball-x-pos bal) (ball-vx bal))
             (+ (ball-y-pos bal) (ball-vy bal))
             (ball-vx bal)
             (ball-vy bal)))

; score : World -> Natural
; Returns the current score.
; Structural decomposition on World
(define (score wld)
  (get-ratio (get-total-area (world-shades wld))
             CANVAS-SIZE))

; TEST
(begin-for-test
  (check-equal?
   (score (make-world
           (list (make-ball 110 214 -2 1))
           (list (make-wall 358 200 400 "vertical" false))
           "vertical"
           1
           (list
            (make-shade (make-posn 358 0) (make-posn 400 400)))))
   10))

; get-ratio: Real Real -> Natural
; RETURN the percentage of shade given the shade size 
; and canvas size
; Functional Composition
(define (get-ratio shds canvas)
  (round (* 100 (/ shds canvas))))
;
(begin-for-test
  (check-equal?
   (get-ratio  
    (* 40 400)
    CANVAS-SIZE)
   10))


; get-total-area: Shades -> Real
; RETURN the total size of the shades
; Functional Composition
(define (get-total-area shds)
  (foldl shade-area 0 shds))

; shade-area: Shade Real -> Real
; Functional Composition
(define (shade-area shd base)
  (cond
    [(shade? shd)(+ (get-size-posns (shade-upleft shd)
                                    (shade-downright shd)) base)]
    [else 0]))

; get-size-posns: Posn Posn -> Real
; Data deComposition on Posn
(define (get-size-posns p1 p2)
  (* (- (posn-x p2) (posn-x p1))
     (- (posn-y p2) (posn-y p1))))

; level : World -> Natural
; Returns the current level.
; data deComposition
(define (level wld)
  (world-level wld))

(begin-for-test
  (check-equal?
   (level 
    (make-world 
     (list (make-ball 140 250 -10 10))
     (list
      (make-wall 260 200 400 "vertical" false)
      (make-wall 319 200 400 "vertical" false))
     "vertical"
     1
     (list
      (make-shade (make-posn 260 0) (make-posn 319 400))
      (make-shade (make-posn 319 0) (make-posn 400 400)))))
   1))

; world-balls : World -> ListOf<Ball>
; Returns all the balls in the given world.
; Stuctural decomposition 
;;; included in the world structure

; mk-ball : Coordinate Coordinate Real Real -> Ball
; Returns a Ball with center at (x,y), with the given velocities.
; A positive x velocity is in the x-increasing direction and vice versa.
; The y velocity is similar.
; Functional Composition
(define (mk-ball x y vx vy)
  (make-ball x y vx vy))

; replace-balls : World ListOf<Ball> -> World
; Replaces the Balls in the given World with the given Balls.
; Data decomposition
(define (replace-balls wld loballs)
  (make-world loballs 
              (world-walls wld)
              (world-orient wld)
              (world-level wld)
              (world-shades wld)))


; get-ball-x : Ball -> Coordinate
; Returns the x position of the Ball's center.
; Structural decompostion on Ball
(define (get-ball-x bal)
  (ball-x-pos bal))

; get-ball-y : Ball -> Coordiate
; Returns the y position of the Ball's center.
; Structural decompostion on Ball
(define (get-ball-y bal)
  (ball-y-pos bal))

; get-balls: World -> Balls
; Returns all the balls in the given world.
; Structural decompostion on World 
(define (get-balls wld)
  (world-balls wld))

; ======================================== KEY-HANDLER ===================================
; key-handler: World KeyEvent -> World
; RETURN a new World with the given World and key Event
; Strucutral decomposition on keyEvent
(define (key-handler wld kev)
  (cond
    [(string=? " " kev)(change-orientation wld)]
    [else wld]))

; TEST
(begin-for-test
  (check-equal?
   (key-handler 
    (make-world 
     (list (make-ball 140 250 -10 10))
     (list (make-wall 260 200 400 "vertical" false)
           (make-wall 319 200 400 "vertical" false))
     "vertical"
     1
     (list (make-shade (make-posn 260 0) (make-posn 319 400))
           (make-shade (make-posn 319 0) (make-posn 400 400))))
    " ")
   (make-world 
     (list (make-ball 140 250 -10 10))
     (list (make-wall 260 200 400 "vertical" false)
           (make-wall 319 200 400 "vertical" false))
     "horizontal"
     1
     (list (make-shade (make-posn 260 0) (make-posn 319 400))
           (make-shade (make-posn 319 0) (make-posn 400 400)))))
  (check-equal?
   (key-handler 
    (make-world 
     (list (make-ball 140 250 -10 10))
     (list (make-wall 260 200 400 "vertical" false)
           (make-wall 319 200 400 "vertical" false))
     "horizontal"
     1
     (list (make-shade (make-posn 260 0) (make-posn 319 400))
           (make-shade (make-posn 319 0) (make-posn 400 400))))
    " ")
   (make-world 
     (list (make-ball 140 250 -10 10))
     (list (make-wall 260 200 400 "vertical" false)
           (make-wall 319 200 400 "vertical" false))
     "vertical"
     1
     (list (make-shade (make-posn 260 0) (make-posn 319 400))
           (make-shade (make-posn 319 0) (make-posn 400 400)))))
  (check-equal?
   (key-handler 
    (make-world 
     (list (make-ball 140 250 -10 10))
     (list (make-wall 260 200 400 "vertical" false)
           (make-wall 319 200 400 "vertical" false))
     "horizontal"
     1
     (list (make-shade (make-posn 260 0) (make-posn 319 400))
           (make-shade (make-posn 319 0) (make-posn 400 400))))
    "a")
   (make-world 
     (list (make-ball 140 250 -10 10))
     (list (make-wall 260 200 400 "vertical" false)
           (make-wall 319 200 400 "vertical" false))
     "horizontal"
     1
     (list (make-shade (make-posn 260 0) (make-posn 319 400))
           (make-shade (make-posn 319 0) (make-posn 400 400))))))
  
; change-orientation: World -> World
; RETURN a new World where the orientation changes
; Strucutural decomposition on World
(define (change-orientation wld)
  (make-world (world-balls wld) 
              (world-walls wld)
              (change-orient (world-orient wld))
              (world-level wld)
              (world-shades wld)))


; change-orient: String -> String
; RETURN a different orientation
; Functional Composition
(define (change-orient orit)
  (if (string=? orit "horizontal")
      "vertical"
      "horizontal"))

; ======================================== MOUSE-HANDLE ==================================
; mouse-handler : World Integer Integer MouseEvent -> World
; Computes the next world after a mouse event.
; WHERE A MouseEvent can be one of:
;    - "button-down"
;    - else
; STRATEGY: Mouse event
(define (mouse-handler wld mx my mev)
  (if (and 
       (equal? mev "button-down")
       (not (click-in-shades? (world-shades wld) mx my))
       (not (active-wall? wld)))
      (world-after-button-down wld mx my)
      wld))

; TEST
(begin-for-test
  (check-equal? 
   (mouse-handler 
    (make-world 
     (list (make-ball 140 250 -10 10))
     (list (make-wall 260 200 400 "vertical" false)
           (make-wall 319 200 400 "vertical" false))
     "horizontal"
     1
     (list (make-shade (make-posn 260 0) (make-posn 319 400))
           (make-shade (make-posn 319 0) (make-posn 400 400))))
    50 100 "button-down")
   (make-world 
     (list (make-ball 140 250 -10 10))
     (list (make-wall 50 100 2 "horizontal" true)
           (make-wall 260 200 400 "vertical" false)
           (make-wall 319 200 400 "vertical" false))
     "horizontal"
     1
     (list (make-shade (make-posn 260 0) (make-posn 319 400))
           (make-shade (make-posn 319 0) (make-posn 400 400)))))
  (check-equal? 
   (mouse-handler 
    (make-world 
     (list (make-ball 140 250 -10 10))
     (list (make-wall 260 200 400 "vertical" false)
           (make-wall 319 200 400 "vertical" false))
     "horizontal"
     1
     (list (make-shade (make-posn 260 0) (make-posn 319 400))
           (make-shade (make-posn 319 0) (make-posn 400 400))))
    350 100 "button-down")
   (make-world 
     (list (make-ball 140 250 -10 10))
     (list (make-wall 260 200 400 "vertical" false)
           (make-wall 319 200 400 "vertical" false))
     "horizontal"
     1
     (list (make-shade (make-posn 260 0) (make-posn 319 400))
           (make-shade (make-posn 319 0) (make-posn 400 400))))))
; click-in-shades? : Shades Real Real -> Boolean
; RETURN true of the click position is in the shadow
; false otherwise
; Functional Composition
(define (click-in-shades? shds x-pos y-pos)
  (local (; is-inside? Shade -> Boolean
          ; true if the posn is in the shade
          (define (is-inside? shd)
            (cond
              [(shade? shd)(check-is-inside 
                            x-pos y-pos (shade-upleft shd)
                            (shade-downright shd))]
              [else false])))
    (ormap is-inside? shds)))

; check-is-inside: Real Real Posn Posn -> Boolean
; true if the x-pos and y-pos is inside the rectangle framed by the two posns
; Strucutural decomposition
(define (check-is-inside x-pos y-pos up-left down-right)
  (and (<= (posn-x up-left) x-pos (posn-x down-right))
       (<= (posn-y up-left) y-pos (posn-y down-right))))

; world-after-button-down: World Real Real -> World
; RETURN  a new world where the walls has a new wall 
; attached being active at the clicking position
; Strucutral Decomposition
(define (world-after-button-down wld x-cor y-cor)
  (make-world (world-balls wld)
              (add-new-wall wld x-cor y-cor)
              (world-orient wld)
              (world-level wld)
              (world-shades wld)))

; add-new-wall: World Real Real -> Walls
; RETURN a list of walls with a new wall attached to the current
; walls
; Strucutral Decomposition
(define (add-new-wall wld x-cor y-cor)
  (cons (make-new-wall (world-orient wld) x-cor y-cor)
        (world-walls wld)))

; make-new-wall: String Real Real -> Wall
; RETURN a new wall with the given orientation and center coordinates
; Functional Composition
(define (make-new-wall ori x-cor y-cor)
  (make-wall x-cor y-cor INITIAL-WALL-LENGTH ori true))

; ======================================== BIG BANG =====================================
; run: World -> World

(define (run w)
  (big-bang w
            (on-tick next-world)
            (on-key key-handler)
            (on-mouse mouse-handler)
            (on-draw render)
            (stop-when end? render-last)))


; ======================================== END GAME ===================================
; end? : World -> Boolean
; RETURN a boolean, true if in the world, the balls
; hits the active wall, false other wise given world
; data decomposition on World

(define (end? wld)
  (hit-active? (world-balls wld)
               (world-walls wld)))

(begin-for-test
  (check-equal?
   (end? (make-world
          (list (make-ball 173 53 9 11))
          (list (make-wall 301 160 194 "vertical" true))
          "vertical"
          1
          empty))
   false))


; hit-active? : Balls Walls -> Boolean 
; RETURN true if one of the balls
; hits the active wall, false other wise 
; if there is no active wall, return false
; data decomposition on Walls
(define (hit-active? loballs lowalls )
  (cond
    [(empty? lowalls) false]
    [(cons? lowalls)(if (hit-active-wall (first lowalls) loballs)
                        true
                        false)]))

; hit-active-wall: Wall Balls -> Boolean
; if the wall is not active or balls not hit it, return false
; if the wall is active and ball hits it, return true
;
(define (hit-active-wall wal loballs)
  (if (and (wall-active wal)
           (balls-hit-wall? wal loballs))
      true
      false))

; balls-hit-wall?: Wall Balls -> Boolean
; RETURN true if the balls hit the wall
; data decomposition on the Balls

(define (balls-hit-wall? wal loballs)
  (cond
    [(empty? loballs) false]
    [(cons? loballs) (or (one-ball-hit-wall? (first loballs) wal)
                         (balls-hit-wall? wal (rest loballs)))]))

(begin-for-test
  (check-equal? 
   (balls-hit-wall? (make-wall 301 160 194 "vertical" true)
                    (list (make-ball 173 53 9 11)))
   false))


; one-ball-hit-wall? Ball Wall -> Boolean
; return true if a ball hits the wall
; Strucutural decomposition 
(define (one-ball-hit-wall? bal wal)
  (if (horizontal? wal)
      (hit-horizontal-wall bal wal)
      (hit-vertical-wall bal wal)))

; horizontal? Wall -> Boolean
; RETURN true of the wall is a horizontal wall 
(define (horizontal? wal)
  (string=? "horizontal" (wall-orientation wal)))

; hit-horizontal-wall: Ball Wall -> Boolean
; RETURN true if the ball hitting horizontal wall
; Strucutural decomposition on Ball and Wall

(define (hit-horizontal-wall bal wal)
  (if (> (ball-y-pos bal) (wall-y-pos wal))
      (and (<= (- (ball-y-pos bal) RADIUS)
               (wall-y-pos wal))
           (ball-x-in-between bal wal))
      (and (>= (+ (ball-y-pos bal) RADIUS)
               (wall-y-pos wal))
           (ball-x-in-between bal wal))))

; TEST
(begin-for-test
  (check-equal? 
   (hit-horizontal-wall 
    (make-ball 50 140 3 5)
    (make-wall 200 (+ RADIUS 140) 300 "horizontal" true))
   true)
  (check-equal? 
   (hit-horizontal-wall 
    (make-ball 50 140 3 -3)
    (make-wall 200 138 300 "horizontal" true))
   true))


; ball-x-in-between: Ball Wall -> boolean
; RETURN true if ball's x coordinate is between the active wall
; Strctural decomposition on Wall and Ball

(define (ball-x-in-between bal wal)
  (> (+ (large-end-wall wal) RADIUS)
     (ball-x-pos bal)
     (- (small-end-wall wal) RADIUS)))

; hit-vertical-wall: Ball Wall -> Boolean
; RETURN true if the ball hitting the vertical wall
; Strucutural decomposition on Ball and Wall

(define (hit-vertical-wall bal wal)
  (if (> (ball-x-pos bal) (wall-x-pos wal))
      (and (<= (- (ball-x-pos bal) RADIUS)
               (wall-x-pos wal))
           (ball-y-in-between bal wal))
      (and (>= (+ (ball-x-pos bal) RADIUS)
               (wall-x-pos wal))
           (ball-y-in-between bal wal))))

; TEST
(begin-for-test
  (check-equal? 
   (hit-vertical-wall (make-ball 173 53 9 11)
                      (make-wall 301 160 194 "vertical" true))
   
   false))

; ball-y-in-between: Ball Wall -> boolean
; RETURN true if ball's y coordinate is between the active wall
; length
; Strctural decomposition on Wall and Ball

(define (ball-y-in-between bal wal)
  (> (+ (wall-y-pos wal)(/ (wall-length wal) 2) RADIUS )
     (ball-y-pos bal)
     (- (wall-y-pos wal) (/ (wall-length wal) 2) RADIUS)))

; test
(begin-for-test
  (check-equal? 
   (ball-y-in-between 
    (make-ball 30 80 3 6)
    (make-wall 40 80 40 "vertical" true))
   true))

