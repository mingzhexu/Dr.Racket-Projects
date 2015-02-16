;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bounce) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "extras.rkt")
(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(define TIME-ON-TASK 35) 

(check-location "03" "bounce.rkt")

(provide INITIAL-WORLD)
(provide next-world)
(provide key-handler)
(provide mouse-handler)
(provide world-ball)
(provide world-paused?)
(provide ticks-since-click)
(provide score)
(provide ball-x)
(provide ball-y)

; ========================= CONSTANTS ===============================                                                                 
                                                           
(define WIDTH 300)
(define HEIGHT 400)
(define RADIUS 20)
(define BALL-X-VEL-START 3)
(define BALL-Y-VEL-START 0)
(define G 1)
(define BALL-START-X (/ WIDTH 2))
(define BALL-START-Y RADIUS)
(define BALL-GROUND-Y (- HEIGHT RADIUS))
(define BOUNCING-COEFF 1)
(define INITIAL-PAUSE false)
(define INITIAL-CLICK-IN 0)
(define INITIAL-SCORE 0)
(define PAUSE-POSN-X 50)
(define PAUSE-POSN-Y 20)
(define SCORE-POSN-X 250)
(define SCORE-POSN-Y 20)

(define IMG-BALL (circle RADIUS "solid" "black"))
(define BACKGROUND (empty-scene WIDTH HEIGHT))

; ========================= BALL ===============================   
; A Ball is (make-ball Posn Real Real Real)
; WHERE:
;    position is (make-posn x y)
;    the x-coordinate and the y-coordinate 
;    of the center of the ball
;    vx is the horizontal velocity of the ball
;    vy is the vertical velocity of the ball
;    acc is the acceleration of the ball
;
(define-struct ball (position vx vy acc))

; TEMPLATE
#;(define (fn-ball curr-ball)
    (...fn-posn(ball-position curr-ball)
        (ball-vx curr-ball)
        (ball-vy curr-ball)
        (ball-acc curr-ball)...))

#;(define (fn-posn position)
    (...(posn-x position)
        (posn-y position)...))

; =============================== WORLD =====================================   

; A World is (make-world Ball Boolean NonNegReal NonNegReal)
; I modified the world for the second assignment which is 
; to pause and click the ball, by adding three fields in this
; structure: Pause and Click-in and Score

; WHERE
;   Pause is a boolean, true if the state is paused, false otherwise
;   Click-in is a number, keep track of the ticks ellapsed 
;   after clicking inside the ball
;   Score is a NonNegReal number to keep track of the number 
;   of clicks before hitting the ground

(define-struct world (ball pause click-in score))

; TEMPLATE:
#;(define (fn-world w)
    ...(fn-ball(world-ball w))...
    ...(world-pause w)...
    ...(world-click-in w)...
    ...(world-score w)...)

; ========================= INITIAL VARIABLE ===============================   

(define INITIAL-POSN (make-posn BALL-START-X BALL-START-Y))

(define INITIAL-BALL (make-ball 
                    INITIAL-POSN
                    BALL-X-VEL-START
                    BALL-Y-VEL-START
                    G))

(define INITIAL-WORLD 
  (make-world INITIAL-BALL INITIAL-PAUSE INITIAL-CLICK-IN INITIAL-SCORE))
                
; ========================= BALLS FOR TESTS  ===============================   
;definition for testing
(define BALL-IN-ROOM-RIGHT-DOWN 
  (make-ball (make-posn 50 100) 3 5 1))
; THE ball is in the room with x velocity to right and y velocity down

(define BALL-IN-ROOM-LEFT-UP 
  (make-ball (make-posn 50 100) -3 -5 1))
; The ball is in the room with x velocity to left and y velocity up

(define BALL-ON-LEFT-WALL 
  (make-ball (make-posn RADIUS 100) 3 5 1))

(define BALL-ON-RIGHT-WALL
  (make-ball (make-posn (- WIDTH RADIUS) 100) 3 5 1))

(define BALL-ON-GROUND-UP-RIGHT
  (make-ball (make-posn 100 BALL-GROUND-Y) 3 -5 1))

; ========================= DRAWING ===============================                      


; render: World -> Image
; GIVEN A World 
; RETURN an Image of the world 
;; WHERE
;    THE ball is on the scene
;    SCORE is up right
;    PAUSE is up left if paused
;    Explosion if the ball is exploded
; STRATEGY: Functional compostion of the image

(define (render wld)
  (place-image
   (draw-score wld)
   SCORE-POSN-X
   SCORE-POSN-Y
   (if (> (world-click-in wld) 0)
       (render-explosion wld)
       (if (world-pause wld)
           (render-pause wld)
           (render-basic-ball wld)))))

; TEST
(begin-for-test
  (check-equal?
   (render (make-world BALL-IN-ROOM-RIGHT-DOWN false 0 1))
   (place-image
    (text "COUNT:  1" 15 "black")
    SCORE-POSN-X SCORE-POSN-Y
    (place-image IMG-BALL
                 50 100 
                 BACKGROUND)) 
   "The ball is inside and scored 1, without being paused or exploded")
  (check-equal?
   (render (make-world BALL-IN-ROOM-LEFT-UP true 0 0))
   (place-image
    (text "COUNT:  0" 15 "black")
    SCORE-POSN-X SCORE-POSN-Y
    (place-image (text "*PAUSED*" 15 "black")
                 PAUSE-POSN-X PAUSE-POSN-Y
                 (place-image IMG-BALL
                              50 100
                              BACKGROUND)))
   "THE BALL is in side the room scored 0, being paused but not exploded")
  (check-equal?
   (render (make-world BALL-ON-LEFT-WALL false 2 0))
   (place-image 
    (text "COUNT:  0" 15 "black")
    SCORE-POSN-X SCORE-POSN-Y
           (place-image 
              (radial-star 12 (* 2 1) (* 4 1)
                           "solid" "yellow")
              RADIUS 100 
              (place-image IMG-BALL
                           RADIUS 100
                           BACKGROUND)))
   "The ball is on the left wall and scored 0, not being paused but exploded"))
     
  
; draw-score: World -> Image
; RETURN a image of the counted score at the top right of the room      
; Functional Composition

(define (draw-score w)
  (beside (text "COUNT:  " 15 "black")
          (text 
           (number->string (world-score w)) 15 "black")))

; render-basic-ball: Wolrd -> Image
; RETURN an image with the ball placed on the background
; according to its position
; Functional Composition

(define (render-basic-ball w)
  (place-image IMG-BALL
               (ball-x (world-ball w))
               (ball-y (world-ball w))
               BACKGROUND))
; test
(begin-for-test
  (check-equal? 
   (render-basic-ball 
    (make-world BALL-ON-RIGHT-WALL false 0 0))
   (place-image IMG-BALL
                (- WIDTH RADIUS) 100
                BACKGROUND)))
                       
; render-pause: World -> Image
; RETURN an image with "PAUSE" on the top left
; Functional composition

(define (render-pause w)
  (place-image (text "*PAUSED*" 15 "black")
               PAUSE-POSN-X PAUSE-POSN-Y
               (render-basic-ball w)))

; render-explosion: World -> Image
; RETURN An Image with the radial-star placed upon the ball
; Functional composition

(define (render-explosion w)
  (place-image (radial-star 12 
                            (* 2 (ticks-since-click w))
                            (* 4 (ticks-since-click w))
                            "solid" "yellow")
               (world-x w)
               (world-y w)
               (render-basic-ball w)))
               
;TEST
(begin-for-test
  (check-equal? 
   (render-explosion 
    (make-world BALL-IN-ROOM-LEFT-UP false 2 1))
   (place-image (radial-star 12 2 4
                             "solid" "yellow")
                50
                100
                (place-image IMG-BALL
                             50 100
                             BACKGROUND))
   "ball is in the room currently having an explosion"))
                             
; ========================= GIVEN DEFINITIONS AND FUNCTIONS =========================                  
               
; world-ball : World -> Ball
; Returns a representation of the ball.
; This is contained in the World defintion, world-ball is a selector 

; ball-x : Ball -> Coordinate
; ball-y : Ball -> Coordinate
; Returns the x or y position of the given Ball.
; STRATEGY: structural decompostion of ball

(define (ball-x curr-ball)
  (posn-x (ball-position curr-ball)))

(define (ball-y curr-ball)
  (posn-y (ball-position curr-ball)))

; world-x: World -> Coordinate
; world-y: World -> Coordinate
; Return the x/y coordinate of the ball given the world
; Structrual decomposition on world and ball

(define (world-x w)
  (ball-x (world-ball w)))

(define (world-y w)
  (ball-y (world-ball w)))

; 1.2.2 Exact Numbers, Inexact Numbers, and Rounding

(define ε 0.001)

; round/ε : Real -> Real
; Rounds x to within ε precision
(define (round/ε x)
  (exact->inexact (* (inexact->exact (round (/ x ε))) ε)))

; Assume any constants used in these tests are as described in the problem.
; addition functions to help test ——————–
 
; world-ball-y : World -> Coordinate
; Return's the y position of w's Ball.
; Strategy: double decomposition(!) on w : World, and Ball
; (Probably) acceptable since the function is for testing 
; and is less likely to be read by others.
(define (world-ball-y w)
  (ball-y (world-ball w)))
 
(begin-for-test
  (check-= (world-ball-y
            (next-world INITIAL-WORLD))
           (new-y BALL-START-Y BALL-Y-VEL-START G 1)
           ε
           "one tick from start")
  (check-= (world-ball-y
            (next-world
             (next-world INITIAL-WORLD)))
           (new-y BALL-START-Y BALL-Y-VEL-START G 2)
           ε
           "two ticks from start"))
 
; next-world-n : Natural World -> World
; Computes the next world n times, starting from w
; This function doesnt fit into any of our current strategies due to its
; recursive nature. (You won't have to write any other recursive
; functions this week.)
(define (next-world-n n w)
  (if (zero? n)
      w
      (next-world-n (sub1 n) (next-world w))))
 
(begin-for-test
  (check-= (world-ball-y
            (next-world-n 26 INITIAL-WORLD))
           (new-y BALL-START-Y BALL-Y-VEL-START G 26)
           ε
           "26 ticks from start")
  
  (check-= (world-ball-y
            (next-world-n 27 INITIAL-WORLD))
           BALL-GROUND-Y
           ε
           "27 ticks from start: ball on ground"))

(begin-for-test
  (check-= (world-ball-y
            (next-world-n 27 (next-world-n 27 INITIAL-WORLD)))
           BALL-START-Y
           (* ε BALL-START-Y)
           "27*2 ticks from start: ball back to start"))

                                                                 
; ========================= NEXT WORLD ===============================   

; next-world: World -> World
; RETURN a new World after one tick given a World
; STRATEGY: Functional Composition

(define (next-world w)
   (next-world-explosion
    (next-world-reset
     (next-world-pause w))))

;TEST
(begin-for-test
  (check-equal?
   (next-world (make-world BALL-IN-ROOM-RIGHT-DOWN false 0 0))
   (make-world 
    (make-ball (make-posn 53 (round/ε 211/2)) 3 6 1) false 0 0)
   "ball is inside the room without being paused or clicked in")
  (check-equal?
   (next-world (make-world BALL-ON-LEFT-WALL true 0 0))
   (make-world BALL-ON-LEFT-WALL true 0 0)
   "the ball is paused so just return the given world")
  (check-equal?
   (next-world (make-world BALL-ON-RIGHT-WALL false 0 0))
   (make-world 
    (make-ball (make-posn (- WIDTH RADIUS) (round/ε 105.5)) -3 6 1) false 0 0)
   "THE ball is flushing the wall and needs to change the horizontal velocity")
  (check-equal?
   (next-world (make-world BALL-ON-RIGHT-WALL false 2 0))
   (make-world 
    (make-ball (make-posn (- WIDTH RADIUS) (round/ε 105.5)) -3 6 1) false 3 0)
   "the ball is clicked inside and is exploding")
  (check-equal?
   (next-world (make-world BALL-ON-GROUND-UP-RIGHT false 0 0))
   (make-world
    (make-ball 
     (make-posn 103 (round/ε (- BALL-GROUND-Y 4.5))) 
     3 (+ (ball-vy BALL-ON-GROUND-UP-RIGHT) 1) 1) false 0 0)
   "THE Ball is on the ground moving up, with score 0 and not paused, not exploded"))
               

; next-world-pause: World -> World
; Given a World
; RETURN A new world with pause implemented
; Functional composition 
(define (next-world-pause w)
  (if (world-pause w)
      w
      (next-world-basic w)))
  
; test
(begin-for-test
  (check-equal?
   (next-world-pause 
    (make-world BALL-IN-ROOM-RIGHT-DOWN true 0 0))
   (make-world BALL-IN-ROOM-RIGHT-DOWN true 0 0)
   "since paused is true, the next world will return the input world")
  (check-equal?
   (next-world-pause
    (make-world BALL-IN-ROOM-LEFT-UP false 0 0))
   (make-world 
    (make-ball (make-posn 47 (round/ε 95.5)) -3 -4 1)
    false 0 0)
   "there is no pause so the ball keep moving"))
    
; next-world-basic: World -> World
; RETURN a new world with the ball moving (generating new ball)
; Structural decomposition on World
      
(define (next-world-basic w)
  (make-world (new-ball (world-ball w))
              (world-pause w)
              (world-click-in w)
              (world-score w)))
  
; TEST
(begin-for-test
  (check-equal? 
   (next-world-basic (make-world BALL-IN-ROOM-LEFT-UP false 0 0))
   (make-world 
    (make-ball (make-posn 47 (round/ε 95.5)) -3 -4 1) false 0 0)
   "the ball is in the room and moving upleft"))
   
; next-world-explosion: World -> World
; RETURN a new world after one tick and check if the 
; click-in is between -1 and 10, keep exploding
; WHERE
;   If the ball is paused, the explosion will freeze
;   and continue after being paused
; STRATEGY: Functional Composition

(define (next-world-explosion w)
  (if (world-pause w)
      w
   (if (< 0 (world-click-in w) 10)
       (make-world (world-ball w)
                   (world-pause w)
                   (+ 1 (world-click-in w))
                   (world-score w))
       (next-world-explosion-helper w))))

; next-world-explosion-helper: World -> World
; RETURN a new world after explosion, set the click back to 0.
; STRATEGY: Structural decomposition

(define (next-world-explosion-helper w)
  (make-world (world-ball w)
              (world-pause w)
              0
              (world-score w)))

; TEST
(begin-for-test
  (check-equal?
   (next-world-explosion 
    (make-world BALL-IN-ROOM-LEFT-UP false 2 1))
   (make-world BALL-IN-ROOM-LEFT-UP false 3 1)
   "the ball is currently experiencing explosion")
  (check-equal?
   (next-world-explosion
    (make-world BALL-IN-ROOM-LEFT-UP false 0 0))
   (make-world BALL-IN-ROOM-LEFT-UP false 0 0)
   "ball is not exploding")
  (check-equal?
   (next-world-explosion
    (make-world BALL-IN-ROOM-LEFT-UP false 10 0))
   (make-world BALL-IN-ROOM-LEFT-UP false 0 0)
   "the ball is finishing explosion")
  (check-equal?
   (next-world-explosion
    (make-world BALL-IN-ROOM-LEFT-UP true 10 0))
   (make-world BALL-IN-ROOM-LEFT-UP true 10 0)
   "the ball is paused while exploding, freeze explosion"))

; next-world-reset: World -> World
; given a world, RETURN the new world with the score reset to 0
; and the click set to 0 after hitting the ground
; STRATEGY: Functional composition

(define (next-world-reset w)
  (if (hit-ground? (world-ball w))
      (make-world (world-ball w)
                  (world-pause w)
                  0
                  0)
       w))

; new-ball: Ball -> Ball
; RETURN a new Ball after moving by one tick
; WHERE the ball will 
;   - bounce if it hits wall or ground
;   - move if it is not going to hit wall or ground
; STRATEGY: Functional compostion

(define (new-ball curr-ball)
  (if (not (hit-wall-or-ground? curr-ball))
      (move curr-ball)
      (bouncing curr-ball)))

; TEST
(begin-for-test
  (check-equal?
   (new-ball BALL-IN-ROOM-RIGHT-DOWN)
   (make-ball (make-posn 53 (round/ε 105.5)) 3 6 1)
   "the ball is in the room moving freely")
  (check-equal?
   (new-ball BALL-ON-GROUND-UP-RIGHT)
   (make-ball 
    (make-posn 103 (round/ε (- HEIGHT RADIUS 4.5)))
    3 -4 1)
   "THE BALL is on the ground and going up the next world 
   would be moving not bouncing")
  (check-equal?
   (new-ball 
    (make-ball (make-posn 100 (- HEIGHT RADIUS 6.5)) 3 6 1))
   (make-ball 
    (make-posn 103 (- HEIGHT RADIUS))
    3 (round/ε (* BOUNCING-COEFF -7)) 1)
   "THE BALL is on the ground and going up the next world 
   would be moving not bouncing"))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HIT ANYWHERE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; hit-wall-or-ground? : Ball -> Boolean
; RETURN true if the ball hits the wall or ground
; and the y velocity is not 0, false otherwise
; STRATEGY: Functional compostion

(define (hit-wall-or-ground? curr-ball)
     (or (hit-wall? curr-ball)
         (hit-ground? curr-ball)))

;test
(begin-for-test
  (check-equal? 
   (hit-wall-or-ground? 
    (make-ball (make-posn RADIUS 50) (- 0 BALL-X-VEL-START) 4 1))
   true "the ball is flushing against the left wall")
  (check-equal? 
   (hit-wall-or-ground? 
    (make-ball (make-posn WIDTH 50) BALL-X-VEL-START 4 1))
   true "the ball is in the right wall")
  (check-equal? 
   (hit-wall-or-ground? 
    (make-ball (make-posn 50 HEIGHT) BALL-X-VEL-START 4 1))
   true "the ball is at the ground going down")
  (check-equal? 
   (hit-wall-or-ground? 
    (make-ball (make-posn 50 100) BALL-X-VEL-START 4 1))
   false "the ball is in the room without touching anywhere"))

; hit-wall? Ball -> Boolean
; RETURN true if the ball hits wall in the next tick
; STRATEGY: Structural decompostion

(define (hit-wall? curr-ball)
  (hit-wall-x? (ball-x curr-ball)
               (ball-vx curr-ball)))
;test
(begin-for-test
  (check-equal? (hit-wall? (make-ball (make-posn 50 100) BALL-X-VEL-START 4 1))
                false "the ball is in the room and not going out")
  (check-equal? (hit-wall? (make-ball (make-posn WIDTH 50) BALL-X-VEL-START 4 1))
                true "the ball is at the right wall and going out"))

; hit-wall-x?: Real Real -> Boolean
; RETURN a true if the next position is hitting/out of the wall 
; STRATEGY: functional composition

(define (hit-wall-x? curr-x vx)
  (or (< (+ curr-x vx) RADIUS)
      (> (+ curr-x vx)(- WIDTH RADIUS))))
;test
(begin-for-test
  (check-equal? (hit-wall-x? RADIUS -3) true "at the left wall and going left"))

; hit-ground?: Ball -> Boolean
; RETURN true if the ball hits the ground, false otherwise
; STRATEGY: Structural decomposition

(define (hit-ground? curr-ball)
  (hit-ground-y? (ball-y curr-ball)
                 (ball-vy curr-ball)
                 (ball-acc curr-ball)))

;test
(begin-for-test
  (check-equal? 
   (hit-ground? 
    (make-ball (make-posn 60 60) 3 4 1)) false "in the room and not going out")
  (check-equal? 
   (hit-ground? 
    (make-ball (make-posn 60 (- HEIGHT RADIUS)) 3 4 1)) true "at the ground and going out"))
   
; hit-ground-y?: Real Real Real-> Boolean
; RETURN a true if the next position is hitting/out of the ground
; STRATEGY: Functional Composition

(define (hit-ground-y? curr-y vy acc)
  (>= (new-y curr-y vy acc 1) (- HEIGHT RADIUS)))
;TEST
(begin-for-test
  (check-equal? (hit-ground-y? (- HEIGHT RADIUS) -4 1)
                false "ball is going up, so it is not going to hit")
  (check-equal? (hit-ground-y? (- HEIGHT RADIUS) 4 1)
                true "ball is going down so it is going out of the room"))
  

; new-y : Real Real Real Real -> Real
; Computes a new y position based on current y position, current y velocity,
; acceleration, and time ellapsed.
; Strategy: function composition
(define (new-y curr-y curr-vel acc t)
  (round/ε (+ curr-y (* curr-vel t) (* 0.5 acc t t))))
; test
(begin-for-test
  (check-= (new-y HEIGHT 2 1 1) (+ HEIGHT 2.5) ε))

; ========================= HIT AND BOUNCE ============================================    

; bouncing: Ball -> Ball
; RETURN a new ball given the current one bouncing back
; WHERE:
;    if hit walls velocity of x change direction
;    if hit ground, velocity of y change direction, reduced to 90%.
;    in both case, we need to calculate the specific time passed 
;    based on which, get real new velocity and real x/y location
; STRATEGY: Functional Composition

(define (bouncing curr-ball)
  (if (hit-wall? curr-ball)
      (flush-against-wall curr-ball)
      (flush-flip-vy curr-ball)))

;test
(begin-for-test
  (check-= 
   (ball-vx (bouncing (make-ball (make-posn (- WIDTH RADIUS) 50) 3 5 1)))
   -3 ε)
  (check-=
   (ball-vy (bouncing (make-ball (make-posn 50 (- HEIGHT RADIUS)) 3 5 1)))
   (* BOUNCING-COEFF -5)ε))


; ========================= BOUNCING AT WALL =============================
; flush-against-wall: Ball -> Ball
; Given the ball is at left wall or right wall
; RETURN a new Ball which is flushing against the wall
; WHERE it's velocity changed
; STRATEGY: Functional Composition

(define (flush-against-wall curr-ball)
  (if (> RADIUS (+ (ball-x curr-ball)
                   (ball-vx curr-ball)))
      (hit-left curr-ball)
      (hit-right curr-ball)))
;test
(begin-for-test
  (check-= 
   (ball-vx (flush-against-wall 
    (make-ball (make-posn RADIUS 50) -3 5 1)))
   3 ε))
   
    

; hit-left: Ball -> Ball
; RETURN A new ball by placing the ball at the left wall and flip the 
; x velocity direction      
; STRATEGY: Structual Decomposition on Ball

(define (hit-left curr-ball)
  (make-ball (make-posn RADIUS (ball-y curr-ball))
             (- 0 (ball-vx curr-ball))
             (new-vy-left curr-ball)
             (ball-acc curr-ball)))
;test
(begin-for-test
  (check-=
   (ball-vx (hit-left (make-ball (make-posn RADIUS 50) -3 5 1)))
   3 ε))

; hit-right: Ball -> Ball
; RETURN A new ball by placing the ball at the right wall and flip the 
; x velocity direction  
; STRATEGY: Structural Decomposition on the Ball
(define (hit-right curr-ball)
  (make-ball (make-posn (- WIDTH RADIUS) 
                        (get-new-y curr-ball))
             (- 0 (ball-vx curr-ball))
             (new-vy-right curr-ball)
             (ball-acc curr-ball)))
;test
(begin-for-test
  (check-=
   (ball-vx (hit-right
    (make-ball (make-posn (- WIDTH RADIUS) 50) 3 5 1)))
   -3 ε))

; get-new-y: Ball -> Real
; RETURN the new y coordinate given the current position 
; given the ball is hitting the wall
; STRATEGY: Structural decompostion on Ball

(define (get-new-y curr-ball)
  (new-y (ball-y curr-ball)
         (ball-vy curr-ball)
         (ball-acc curr-ball)
         1))

;test
(begin-for-test
  (check-equal?
   (get-new-y 
    (make-ball (make-posn (- WIDTH RADIUS) 100) 3 6 1)) (round/ε 106.5)))
   

; new-vy-left: Ball -> Real
; RETURN A real number which is the new velocity of y if hit left
; STRATEGY: Functional Composition 
(define (new-vy-left curr-ball)
  (+ (ball-vy curr-ball)
     (ball-acc curr-ball)))
        

; new-vy-right: Ball -> Real
; RETURN a real number which is the velocity of y given the ball hit right wall
; Functional composition
(define (new-vy-right curr-ball)
  (+ (ball-vy curr-ball)
     (ball-acc curr-ball)))

; velocity-change: Ball -> Real
; Given a ball
; RETURN the change of y velocity given the period of time
; Functional Composition
(define (velocity-change curr-ball)
  (* (ball-acc curr-ball)
     (get-time-hitting-wall curr-ball)))

; test
(begin-for-test
  (check-=
   (velocity-change (make-ball (make-posn (- WIDTH RADIUS) 50) 3 5 1))
   0 ε "since the ball is flushing against the wall, the time passed will be 0
     there for the changed velocity will be 0"))

; get-time-hitting-wall: Ball -> NonNegReal
; RETURN the time ellapsed for the ball to hit the wall
; Functional composition
(define (get-time-hitting-wall curr-ball)  
  (get-time-wall (ball-vx curr-ball)
                 (- (- WIDTH RADIUS)(ball-x curr-ball))))

; get-time-wall: Real Real -> Real
; RETURN the time passed before the ball bouncing back
; Functional Composition
(define (get-time-wall vx dist)
  (if (> (/ dist vx) 0)
      (round/ε (/ dist vx))
      (round/ε (- 0 (/ dist vx)))))
;test
(begin-for-test
  (check-= (get-time-wall 3 2)
                2/3 ε)
  (check-= (get-time-wall -3 2)
                2/3 ε))
   
;;;;; ========================= BOUNCING AT GROUND =========================    

; flush-flip-vy: Ball-> Ball
; RETURN A Ball that flushs against the ground and the y-velocity flips 
;
(define (flush-flip-vy curr-ball)
  (make-ball (new-ball-posn-at-ground curr-ball)
             (ball-vx curr-ball)
             (vy-bounce-ground curr-ball)
             (ball-acc curr-ball)))
;test
(begin-for-test 
  (check-= 
   (ball-x (flush-flip-vy 
           (make-ball (make-posn 50 (- HEIGHT RADIUS)) 3 5 1))) 
   53 ε)
  (check-= 
   (ball-y (flush-flip-vy 
            (make-ball (make-posn 50 (- HEIGHT RADIUS)) 3 5 1))) 
   BALL-GROUND-Y ε))
  
; new-ball-posn-at-ground: Ball -> Posn
; RETURN A Posn which is the center position of the ball 
; which is at ground given a ball hitting the ground

(define (new-ball-posn-at-ground curr-ball)
  (make-posn (+ (ball-x curr-ball)(ball-vx curr-ball))
             (- HEIGHT RADIUS)))

; vy-bounce-ground: Ball -> Real
(define (vy-bounce-ground curr-ball)
  (new-vy-bounce (ball-y curr-ball)
                 (ball-vy curr-ball)
                 (ball-acc curr-ball)))
    

; new-vy-bounce: Real Real Real -> Real
; GIVEN: the original y-coordinate, velocity of y, and the acceleration
; RETURN the new velocity


(define (new-vy-bounce curr-y vy acc)
  (round/ε (- 0 (* (+ vy (* acc (get-time curr-y vy acc)))
                   BOUNCING-COEFF))))

; get-time: Real Real Real -> Real
; INTERP: since the velocity when the ball hit the ground 
; will always be positive, so the sqrt opertation should only 
; be plus not minus since the time we need is always positive number
(define (get-time curr-y vy acc)
   (round/ε (/ (- (sqrt (- (expt vy 2)(* 2 acc (c-in-equation curr-y)))) vy) acc)))
;TEST
(begin-for-test
  (check-= (get-time BALL-GROUND-Y 10 1) 
           0 ε)
  (check-= (get-time (- BALL-GROUND-Y 10) 10 1)
           (- (sqrt 120) 10) ε))

(define (c-in-equation curr-y)
  (- curr-y BALL-GROUND-Y))
;test
(begin-for-test
  (check-equal? (c-in-equation (- HEIGHT RADIUS)) 0)
  (check-equal? (c-in-equation (- HEIGHT RADIUS 10)) -10))

;;; ========================= MOVE FREELY ===========================

; move: Ball -> Ball
; RETURN a new Ball without bouncing back

(define (move curr-ball)
  (make-ball (new-position curr-ball)
             (ball-vx curr-ball)
             (new-vy-move curr-ball)
             (ball-acc curr-ball)))

(begin-for-test
  (check-equal?
   (move (make-ball (make-posn 50 50) 3 5 1))
   (make-ball (make-posn 53 (round/ε 55.5)) 3 6 1))
  (check-equal?
   (move (make-ball (make-posn 50 50) -3 5 1))
   (make-ball (make-posn 47 (round/ε 111/2)) -3 6 1))
  (check-equal?
   (move (make-ball (make-posn 50 (- HEIGHT RADIUS)) 3 0 1))
   (make-ball (make-posn 53 (- HEIGHT RADIUS)) 3 0 1)))


; new-position: Ball -> Posn
; RETURN A new Posn given the ball
(define (new-position curr-ball)
  (make-posn (new-position-x curr-ball)
             (new-position-y curr-ball)))

; new-position-x: Ball -> Real
; RETURN the new x-coordinate based on the ball's 
; horizontal velocity and current x
(define (new-position-x curr-ball)
  (+ (ball-x curr-ball) (* 1 (ball-vx curr-ball))))

; new-position-y: Ball -> Real
; RETURN the new y-coordinate based on the ball's
; Vertical velocity and acceleration
(define (new-position-y curr-ball)
  (if (and (= 0 (round (ball-vy curr-ball)))
           (= (ball-y curr-ball) BALL-GROUND-Y))
      BALL-GROUND-Y
      (new-y (ball-y curr-ball)
             (ball-vy curr-ball)
             (ball-acc curr-ball)
             1)))

; new-vy-move: Ball -> Real
(define (new-vy-move curr-ball)
  (if (and (= 0 (round (ball-vy curr-ball)))
           (= (ball-y curr-ball) BALL-GROUND-Y))
      0
      (+ (ball-vy curr-ball)(* (ball-acc curr-ball) 1))))

  
;;; ========================= KEY-HANDLER =================================  
; key-handler: World String-> World
; Computes the next world after a key press.
; GIVEN: a world w and a String
; RETURNS: the world that should follow the given world
; after the given key event.
; on "p", ignore all otheres
; EXAMPLES: see tests below
; STRATEGY: Cases on kev : KeyEvent

(define (key-handler w kev)
  (cond
    [(or (key=? kev "p")
         (key=? kev "P"))
     (world-with-pause-toggled w)]
    [else w]))

; test
(begin-for-test
  (check-equal?
   (key-handler 
    (make-world BALL-IN-ROOM-RIGHT-DOWN false 0 0) "p")
   (make-world BALL-IN-ROOM-RIGHT-DOWN true 0 0)
   "the original ball is not paused, with 'p' given, it will be paused")
  (check-equal?
   (key-handler
    (make-world BALL-IN-ROOM-RIGHT-DOWN true 0 0) "P")
   (make-world BALL-IN-ROOM-RIGHT-DOWN false 0 0)
   "the original ball is paused, with 'p' given, it will not be paused")
  (check-equal?
   (key-handler
    (make-world BALL-IN-ROOM-LEFT-UP true 0 0) "a")
   (make-world BALL-IN-ROOM-LEFT-UP true 0 0)
   "the original ball is paused, with 'a' given, a new world will generate"))
  

;; world-with-pause-toggled : World -> World
;; RETURNS: a world just like the given one, but with pause toggled
;; STRATEGY: structural decomposition on w : World
(define (world-with-pause-toggled w)
  (make-world
       (world-ball w)
       (not (world-pause w))
       (world-click-in w)
       (world-score w)))

; world-paused? : World -> Boolean
; Indicates whether the game is paused.

(define (world-paused? w)
  (world-pause w))
; test
(begin-for-test
  (check-equal? 
   (world-paused? (make-world BALL-IN-ROOM-RIGHT-DOWN false 0 0))
   false "the world is not paused")
  (check-equal? 
   (world-paused? (make-world BALL-IN-ROOM-RIGHT-DOWN true 0 0))
   true "the world is paused"))

;;;;; ========================= MOUSE-HANDLER =================================

; mouse-handler : World Integer Integer MouseEvent -> World
; Computes the next world after a mouse event.
; WHERE A MouseEvent can be one of:
;    - "button-down"
;    - else
; STRATEGY: Mouse event
(define (mouse-handler w mx my mev)
  (if (and 
       (equal? mev "button-down")
       (click-inside? (world-ball w) mx my))
      (world-after-button-down w)
      w))

;TEST
(begin-for-test
  (check-equal? 
   (mouse-handler 
    (make-world BALL-IN-ROOM-RIGHT-DOWN true 0 0) 51 101 "button-down")
   (make-world 
    (make-ball (make-posn 50 100) 3 5 1) true 0 0)
   "the world is not being exploded and the click is inside, BUT since
    the ball is paused, the click does not count")
  
  (check-equal? 
   (mouse-handler 
    (make-world BALL-IN-ROOM-RIGHT-DOWN true 0 0) 10 101 "button-down")
   (make-world BALL-IN-ROOM-RIGHT-DOWN true 0 0)
   "the world is not being exploded and the click is outside")
  
  (check-equal? 
   (mouse-handler 
    (make-world BALL-IN-ROOM-RIGHT-DOWN false 5 1) 50 101 "button-down")
   (make-world 
    (make-ball (make-posn 50 100) 3 -10 1) false 1 2)
   "the world is being exploded and the click is inside"))
  

; world-after-button-down: World -> World
; RETURN a new world given the ball is clicked
(define (world-after-button-down w)
  (if (and (= (world-click-in w) 0)
           (not (world-pause w)))
      (make-world (ball-explosion (world-ball w))
                  (world-pause w)
                  (+ 1 (world-click-in w))
                  (+ 1 (world-score w)))
       (stop-and-start-new-explosion w)))

; stop-and-start-new-explosion: World -> World
; RETURN A NEW world with previous explosion eliminated 
; and generate a new one with click set to 1 and score increments

(define (stop-and-start-new-explosion w)
  (if (world-pause w)
      w
      (make-world (ball-explosion (world-ball w))
                  (world-pause w)
                  1
                  (+ 1 (world-score w)))))
  
; ball-explosion: Ball -> Ball
; GIVEN a ball and 
; RETURN A new ball
; WHERE
;   - If the ball is moving upward, 
; increase the ball's velocity by 10pixel/tick
;   - If the ball is moving downwards, 
; change the ball's velocity to 10 upward

(define (ball-explosion curr-ball)
  (if (> (ball-vy curr-ball) 0)
      (ball-change-speed curr-ball)
      (ball-increase-speed curr-ball)))

(define (ball-increase-speed curr-ball)
  (make-ball (ball-position curr-ball)
             (ball-vx curr-ball)
             (- (ball-vy curr-ball) 10)
             (ball-acc curr-ball)))

(define (ball-change-speed curr-ball)
  (make-ball (ball-position curr-ball)
             (ball-vx curr-ball)
             -10
             (ball-acc curr-ball)))

; click-inside?: Ball Integer Integer -> Boolean
(define (click-inside? curr-ball mx my)
  (< (sqrt (+ (expt (- mx (ball-x curr-ball)) 2)
           (expt (- my (ball-y curr-ball)) 2))) RADIUS))

; score : World -> NonNegInt
; Returns the current score.
 (define (score w)
   (world-score w))
 
; TEST
(begin-for-test
  (check-equal?
   (score (make-world BALL-IN-ROOM-RIGHT-DOWN false 0 2)) 2)) 
 
;;; ========================= BIG-BANG MAIN =================================


(define (run wld)
  (big-bang wld
            (on-tick next-world)
            (on-key key-handler)
            (on-mouse mouse-handler)
            (on-draw render)))



;;; ========================= TEST TICKS ===================================
; ticks-since-click : World -> [0 10)
; Returns the number of ticks since the last explosion, if there's
; currently one. 0 means no explosion.
(define (ticks-since-click w)
   (if(> (world-click-in w) 0)
      (- (world-click-in w) 1)
      (world-click-in w)))

(begin-for-test
  (check-true (zero? (ticks-since-click INITIAL-WORLD))
              "no initial explosion")
  (check-true (zero?
               (ticks-since-click
                (mouse-handler
                 INITIAL-WORLD BALL-START-X BALL-START-Y "button-down")))
              "explosion just started")
  (check-equal? (ticks-since-click
                 (next-world
                  (mouse-handler
                   INITIAL-WORLD BALL-START-X BALL-START-Y "button-down")))
                1
                "1 tick after explosion")
  (check-equal? (ticks-since-click
                 (next-world-n 9
                  (mouse-handler
                   INITIAL-WORLD BALL-START-X BALL-START-Y "button-down")))
                9
                "9 ticks after explosion")
  (check-true (zero?
               (ticks-since-click
                (next-world-n 10
                 (mouse-handler
                  INITIAL-WORLD BALL-START-X BALL-START-Y "button-down"))))
              "10 ticks after explosion: explosion ended"))

;; ========================= ALTERNATIVE DEFINITION ===============================

; Data definition 
; My definition for World and Ball are both 4 fields, instead of 7 fields
; in one structure.
; For example:
; - for World
; another defintion for world can be 
; (make-world ball-position vx vy acc paused click-in score)
; this definition can put all the features into the world, so wheneven I
; want to get the field, it's easy to select, no need to double decompose
; HOWEVER, Given too many fields in one structure, if I want to have 
; (make-world * * * * * * *), it will be a pain since there are so many fields in it.
; - for Ball
; If I store features such as paused inside the ball, same things happen. There 
; will be too many fields in the Ball structure.


; My definition on World (make-world Ball pause click-in score)
; The data types are as Ball, boolean, NonNegReal, NonNegReal; 
; 
; I can also use a boolean to represent Click-in instead of using a number
; However, using a number to represent the number of ticks after clicked 
; inside the ball is more convenient. Otherwise, using a boolean to 
; represent the click-in, I will have to include another field 
; number-of-ticks-after-click-in inside the world structure. 

; My definition for Ball (make-ball position vx vy acc)
; I can use alternative definition for position instead of a Posn. For example
; (make-ball x y vx vy acc). It will be easier to access the x and y coordinate
; while I will have 5 fields instead of 4, which might be a little less concise.




