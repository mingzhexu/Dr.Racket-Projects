;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname worms) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "extras.rkt")
(require 2htdp/image)
(require 2htdp/universe)
(require rackunit)

(define TIME-ON-TASK 10)

(provide INITIAL-WORLD)
(provide next-world)
(provide key-handler)
(provide end?)
(provide world-worm)
(provide create-worm)
(provide worm-length)
(provide worm-head-x)
(provide worm-head-y)
(provide replace-food)
(provide replace-worm)
(provide posns-overlap?)
;(check-location "04" "worms.rkt")

; ============================ CONSTANTS ===============================

(define HEIGHT 300)
(define WIDTH 300)
(define SEGMENT-DIAMETER 10)
(define FOOD-DIAMETER 10)
(define SEGMENT-RADIUS (/ FOOD-DIAMETER 2))
(define FOOD-RADIUS (/ FOOD-DIAMETER 2))
(define BACKGROUND (empty-scene HEIGHT WIDTH))

(define FOOD-IMAGE (circle FOOD-RADIUS "solid" "green"))
(define SEGMENT-IMAGE (circle SEGMENT-RADIUS "solid" "red"))

(define FONT-SIZE 15)
(define FONT-COLOR "black")
(define HIT-WALL-MESSAGE (text "worm hits wall: " FONT-SIZE FONT-COLOR))
(define HIT-ITSELF-MESSAGE (text "run into itself: " FONT-SIZE FONT-COLOR))
(define MESSAGE-POSN-X (/ WIDTH 4))
(define MESSAGE-POSN-Y (- HEIGHT FONT-SIZE))

; ============================ DATA DEFINITION ===============================
; A PosInteger is a positive integer

; Posn is (make-posn x y) 
; WHERE 
;  - x is a positive integer (can't go out of canvas), representing x coordinate
;  - y is also a positive integer, representing y coordinate

#; (define (fn-posn aposn)
     (...(posn-x aposn)...
         (posn-y aposn)...))

; A Segment is a Posn

; Body is
;  - a list of Segments (ListOfSegs, or ListOfPosns as provided)
#; (define (fn-body bd)
     (cond
       [(empty? bd) ...]
       [(cons? bd) ... (first bd)
                   ... (fn-body (rest bd)) ...]))

; Direction is a string; it is one of 
;  - "left"
;  - "right"
;  - "up"
;  - "down"
#;(define (fn-dir dir)
    (cond
      [(string=? dir "left") ...]
      [(string=? dir "right") ...]
      [(string=? dir "up") ...]
      [(string=? dir "down") ...]))


; Worm is (make-worm Body Direction)
(define-struct worm (body direction))

#; (define (fn-worm wrm)
     (... (fn-body (worm-body wrm)...)
          (fn-direction (worm-direction wrm)...)))

; Food is a Posn; the posn representing it's position

; A World is (make-world food worm), contains a food and a worm
(define-struct world (food worm))

#; (define (fn-world wld)
     (... (fn-posn (world-food wld)...)
          (fn-worm (world-worm wld)...)))



; ============================ DRAWING ===============================
; render:  World -> Image
; Given the current world, return an image putting the worm and 
; food on the canvas
; STRATEGY: Strucutral decomposition on World

(define (render wld)
  (render-worm (world-worm wld)
               (render-food (world-food wld) BACKGROUND)))

; TEST
(begin-for-test
  (check-equal? (render (make-world (make-posn 40 50)
                                    (make-worm (list (make-posn 50 80) 
                                                     (make-posn 40 80))
                                               "right")))
                (render-body (list (make-posn 50 80) 
                                   (make-posn 40 80))
                             (render-food (make-posn 40 50)
                                          BACKGROUND))))

; render-worm: Worm Image -> Image
; RETURN an image with the worm pl
; STRATEGY: Structural decomposition
(define (render-worm wm img)
  (render-body (worm-body wm) img))

; TEST
(begin-for-test (render-worm (make-worm (list (make-posn 50 80) 
                                              (make-posn 40 80))
                                        "right") BACKGROUND)
                (render-body (list (make-posn 50 80) 
                                   (make-posn 40 80)) BACKGROUND))

; render-body: Body Image -> Image
; RETURN the worm's body placed in the image
; STRATEGY: Structrual decomposition on Body
(define (render-body bd img)
  (cond
    [(empty? bd) img]
    [(cons? bd) (render-segment (first bd)
                                (render-body (rest bd) img))]))
; TEST
(begin-for-test
  (check-equal? (render-body (list (make-posn 50 80) 
                                   (make-posn 40 80)) BACKGROUND)
                (place-image SEGMENT-IMAGE
                             50 80
                             (place-image SEGMENT-IMAGE
                                          40 80
                                          BACKGROUND))))
; render-segment: Posn Image -> Image
; RETURN a segment of the worm image placed on given image
; STRATEGY: Structural decomposition on the Segment
(define (render-segment seg img)
  (place-image SEGMENT-IMAGE
               (posn-x seg)
               (posn-y seg)
               img))

; TEST
(begin-for-test
  (check-equal? (render-segment (make-posn 50 80) 
                                BACKGROUND)
                (place-image SEGMENT-IMAGE
                             50 80
                             BACKGROUND)))

; render-food: Food Image -> Image
; RETURN  an image with the food placed on the image
; STRATEGY: Structural decomposition on the Food
(define (render-food fd img)
  (place-image FOOD-IMAGE
               (posn-x fd)
               (posn-y fd)
               img))

; TEST
(begin-for-test
  (check-equal? (render-food (make-posn 40 80)
                             BACKGROUND)
                (place-image FOOD-IMAGE
                             40 80
                             BACKGROUND)))

; render-last: World -> Image
; RETURN the world stopped and a message showed
; up with how long the worm has grown so far
; STRATEGY: Functional Composition
(define (render-last wld)
  (if (hit-wall? (world-worm wld))
      (render-hit-wall wld)
      (render-hit-itself wld)))
; TEST
(begin-for-test
  (check-equal? (render-last 
                 (make-world (make-posn 50 80)
                             (make-worm 
                              (list (make-posn 395 200)) "right")))
                (render-hit-wall 
                 (make-world (make-posn 50 80)
                             (make-worm 
                              (list (make-posn 395 200)) "right"))))
  (check-equal? 
   (render-last
    (make-world (make-posn 50 100)
                (make-worm (list (make-posn 75 95)
                                 (make-posn 85 95)
                                 (make-posn 75 95)
                                 (make-posn 65 95)
                                 (make-posn 55 95)) "up")))
   (render-hit-itself 
    (make-world (make-posn 50 100)
                (make-worm (list (make-posn 75 95)
                                 (make-posn 85 95)
                                 (make-posn 75 95)
                                 (make-posn 65 95)
                                 (make-posn 55 95)) "up")))))


; render-hit-wall: World -> Image
; RETURN an image given the world with 
; the worm hitting the wall and shows
; the text as well as the length of the worm.
; STRATEGY: Functional composition
(define (render-hit-wall wld)
  (place-image (render-wall-message-and-length wld)
               MESSAGE-POSN-X
               MESSAGE-POSN-Y
               (render wld)))

; render-hit-itself: World -> Image
; RETURN an image given the world with 
; the worm hitting itself and shows
; the text as well as the length of the worm.
; STRATEGY: Functional composition
(define (render-hit-itself wld)
  (place-image (render-self-message-and-length wld)
               MESSAGE-POSN-X
               MESSAGE-POSN-Y
               (render wld)))

; TEST
(begin-for-test
  (check-equal? 
   (render-hit-itself 
    (make-world
     (make-posn 50 100)
     (make-worm (list (make-posn 75 95)
                      (make-posn 85 95)
                      (make-posn 75 95)
                      (make-posn 65 95)
                      (make-posn 55 95)) "up")))
   (place-image
    (render-self-message-and-length 
     (make-world
      (make-posn 50 100)
      (make-worm (list (make-posn 75 95)
                       (make-posn 85 95)
                       (make-posn 75 95)
                       (make-posn 65 95)
                       (make-posn 55 95)) "up")))
    MESSAGE-POSN-X
    MESSAGE-POSN-Y
    (render (make-world
             (make-posn 50 100)
             (make-worm (list (make-posn 75 95)
                              (make-posn 85 95)
                              (make-posn 75 95)
                              (make-posn 65 95)
                              (make-posn 55 95)) "up"))))))

; render-wall-message-and-length:  World -> Image
; RETURN an image with the message and length drawed
; STRATEGY: Functional composition 
(define (render-wall-message-and-length wld)
  (beside HIT-WALL-MESSAGE
          (render-length wld)))
; TEST
(begin-for-test
  (check-equal? (render-wall-message-and-length 
                 (make-world 
                  (make-posn 40 20)
                  (make-worm (list (make-posn 305 200)
                                   (make-posn 295 200))
                             "right")))
                (beside HIT-WALL-MESSAGE 
                        (text "2" FONT-SIZE FONT-COLOR))))

; render-self-message-and-length:  World -> Image
; RETURN an image with the message and length drawed
; STRATEGY: Functional composition 

(define (render-self-message-and-length wld)
  (beside HIT-ITSELF-MESSAGE
          (render-length wld)))

; TEST
(begin-for-test
  (check-equal? (render-self-message-and-length 
                 (make-world 
                  (make-posn 40 20)
                  (make-worm (list (make-posn 75 95)
                                   (make-posn 85 95)
                                   (make-posn 75 95)
                                   (make-posn 65 95)
                                   (make-posn 55 95))
                             "left")))
                (beside HIT-ITSELF-MESSAGE 
                        (text "5" FONT-SIZE FONT-COLOR))))


; render-length: World -> Image
; RETURN an image with the length of worm displayed
; STRATEGY: structural decompostion on World 
; (worm-length is a function)

(define (render-length wld)
  (text (number->string (worm-length (world-worm wld)))
        FONT-SIZE FONT-COLOR))

; ============================ INTERACTIONS ===============================

; next-world: World -> World
; RETURN a new world after the tick
; Functional Composition

(define (next-world wld)
  (if (eating? wld)
      (worm-move (eat-and-grow wld))
      (worm-move wld)))

; TEST
(begin-for-test
  (check-equal? 
   (next-world 
    (make-world (make-posn 55 85)
                (make-worm (list (make-posn 285 285)) "right")))
   (make-world (make-posn 55 85)
               (make-worm (list (make-posn 295 285)) "right"))))
(check-random 
 (next-world (make-world (make-posn 55 85) 
                         (make-worm (list (make-posn 55 85)
                                          (make-posn 65 85)) "left")))
 (make-world (random-food (list (make-posn 35 85)
                                (make-posn 45 85)
                                (make-posn 55 85)))
             (make-worm (list (make-posn 35 85)
                              (make-posn 45 85)
                              (make-posn 55 85)) "left")))



; ============================ EAT-&-GROW ===============================
; eating?: World -> Boolean
; RETURN true if the worm eat food, false otherwise
; STRATEGY: Structural decomposition

(define (eating? wld)
  (posns-overlap? (world-food wld)
                  (get-body (world-worm wld))))

; TEST
(begin-for-test
  (check-equal? (eating? (make-world 
                          (make-posn 50 80)
                          (make-worm (list (make-posn 285 285)) "right")))
                false)
  (check-equal? (eating? (make-world 
                          (make-posn 50 80)
                          (make-worm (list (make-posn 50 80)) "right")))
                true))

; get-body: Worm -> Body
; RETURN the body of the worm
; Structural decomposition

(define (get-body wm)
  (worm-body wm))

; TEST

(begin-for-test
  (check-equal? (get-body (make-worm (list (make-posn 20 50)
                                           (make-posn 30 50))
                                     "left"))
                (list (make-posn 20 50)
                      (make-posn 30 50))))

; eat-and-grow: World -> World
; RETURN A new world after the worm eat the food
; WHERE
;   - new Food will be generated to replace the old food
;   - Worm will have a new head at the same time 
;     not elliminate the last segment
; Structural decomposition

(define (eat-and-grow wld)
  (make-world (random-food (get-body (world-worm wld)))
              (worm-grow (world-worm wld))))

; TEST
(check-random (eat-and-grow (make-world 
                             (make-posn 50 80)
                             (make-worm (list (make-posn 50 80)) "right")))
              (make-world (random-food (list (make-posn 50 80)))
                          (make-worm (list (make-posn 60 80) 
                                           (make-posn 50 80)) "right")))

; worm-grow: Worm -> Worm
; RETURN the new worm with a new head in the direction
; STRATEGY: Structural decomposition

(define (worm-grow wm)
  (make-worm (create-new-head (worm-direction wm) 
                              (worm-body wm))
             (worm-direction wm)))

; TEST: 
(begin-for-test
  (check-equal? (worm-grow 
                 (make-worm (list (make-posn 30 50)
                                  (make-posn 40 50))
                            "left"))
                (make-worm (list (make-posn 20 50)
                                 (make-posn 30 50)
                                 (make-posn 40 50))
                           "left")))



; create-new-head: String ListofSegs -> ListofSegs
; RETURN A new Body with a new head attached in front
; Structural decomposition
(define (create-new-head dir bd)
  (cons (add-new-head dir (first bd)) bd))

; TEST
(begin-for-test
  (check-equal? (create-new-head "left" (list (make-posn 30 50)
                                              (make-posn 40 50)))
                (list (make-posn 20 50)
                      (make-posn 30 50)
                      (make-posn 40 50))))

; add-new-head: String Posn -> Posn
; RETURN the new head with the given direction and the posn
; Structural decomposition
(define (add-new-head dir head)
  (cond
    [(string=? dir "left") (make-posn-left head)]
    [(string=? dir "right") (make-posn-right head)]
    [(string=? dir "up") (make-posn-up head)]
    [(string=? dir "down") (make-posn-down head)]))
;
; TEST
(begin-for-test
  (check-equal? (add-new-head "left" (make-posn 30 50))
                (make-posn 20 50))
  (check-equal? (add-new-head "right" (make-posn 30 50))
                (make-posn 40 50))
  (check-equal? (add-new-head "up" (make-posn 30 50))
                (make-posn 30 40))
  (check-equal? (add-new-head "down" (make-posn 30 50))
                (make-posn 30 60)))

; make-posn-left: Posn -> Posn
; RETURN the new Posn which is left to teh given posn
; Structural decomposition
(define (make-posn-left curr-head)
  (make-posn (- (posn-x curr-head) SEGMENT-DIAMETER) 
             (posn-y curr-head)))

; make-posn-right: Posn -> Posn
; RETURN the new Posn which is right to teh given posn
; Structural decomposition
(define (make-posn-right curr-head)
  (make-posn (+ (posn-x curr-head) SEGMENT-DIAMETER) 
             (posn-y curr-head)))

; make-posn-up: Posn -> Posn
; RETURN the new Posn which is above the given posn
; Structural decomposition
(define (make-posn-up curr-head)
  (make-posn (posn-x curr-head) 
             (- (posn-y curr-head) SEGMENT-DIAMETER)))

; make-posn-down: Posn -> Posn
; RETURN the new Posn which is bellow the given posn
; Structural decomposition
(define (make-posn-down curr-head)
  (make-posn (posn-x curr-head) 
             (+ (posn-y curr-head) SEGMENT-DIAMETER)))

; ============================ WORM MOVE ===============================

; worm-move: World -> World
; RETURN a new world with the worm move forward
; WHERE
;   - Worm will have a new head
;   - Worm's tail element will be dismissed
; decomposition on teh world
(define (worm-move wld)
  (make-world (world-food wld)
              (new-worm (world-worm wld))))

; TEST
(begin-for-test
  (check-equal? (worm-move 
                 (make-world (make-posn 30 50)
                             (make-worm (list (make-posn 40 50))
                                        "right")))
                (make-world (make-posn 30 50)
                            (make-worm (list (make-posn 50 50))
                                       "right"))))
; new-worm: Worm -> Worm
; GIVEN a worm
; RETURN a new worm with a new head
; and Worm's tail element will be dismissed
; structural decomposition
(define (new-worm wm)
  (make-worm (worm-move-with-direction (worm-direction wm)
                                       (worm-body wm))
             (worm-direction wm)))

; TEST
(begin-for-test
  (check-equal? (new-worm (make-worm
                           (list (make-posn 30 50)
                                 (make-posn 40 50)
                                 (make-posn 50 50))
                           "left"))
                (make-worm (list (make-posn 20 50)
                                 (make-posn 30 50)
                                 (make-posn 40 50))
                           "left")))
; worm-move-with-direction: Direction Body -> Body
; RETURN a mew body based on the direction and the current body
; Structural decomposition on the Body
;
(define (worm-move-with-direction dir bd)
  (cons (add-new-head dir (first bd))
        (remove-last-element bd)))

; TEST
(begin-for-test
  (check-equal? (worm-move-with-direction 
                 "left" 
                 (list (make-posn 20 50)
                       (make-posn 30 50)))
                (list (make-posn 10 50)
                      (make-posn 20 50))))

; remove-last-element: Body -> Body
; RETURN a new body with the last element eliminated
; Strucutral decomposition on Body
(define (remove-last-element bd)
  (cond
    [(empty? (rest bd)) empty]
    [(cons? bd)(cons (first bd)
                     (remove-last-element (rest bd)))]))

; TEST
(begin-for-test
  (check-equal? (remove-last-element 
                 (list (make-posn 20 50)
                       (make-posn 30 50)
                       (make-posn 40 50)))
                (list (make-posn 20 50)
                      (make-posn 30 50))))


; world-worm : World -> Worm
; Returns a representation of the Worm in the game.
; Structural decomposition
; This one is contained in the structure and (world-worm wld) is a selector

; create-worm : ListOfPosn -> Worm
; WHERE: the list of posns are contiguous and form a valid worm
; RETURN a new worm given the old one with the worm moving forward
(define (create-worm bd)
  (make-worm  bd "right"))

; TEST
(begin-for-test 
  (check-equal? (create-worm (list (make-posn 45 75)))
                (make-worm (list (make-posn 45 75)) "right")))
; worm-length : Worm -> PosInt
; Returns the number of segments in the given worm.
; Structural decomposition

(define (worm-length wrm)
  (length (worm-body wrm)))

; TEST
(begin-for-test
  (check-equal? (worm-length 
                 (make-worm 
                  (list (make-posn 40 60)
                        (make-posn 50 60))
                  "left"))
                2))


; worm-head-x : Worm -> Coordinate
; Returns the x position of the center of the worm's lead segment.
; structural decompostiion on worm
(define (worm-head-x wm)
  (get-x (worm-body wm)))

; get-x: Body -> Integer
; RETURN the x position of teh center of the worm's lead segment
; Structural decomposition on body

(define (get-x bd)
  (get-posn-x (first bd)))

; get-posn-x: Posn -> Integer
; RETURN the x position of teh center of the worm's lead segment
; structural decomposition on posn
(define (get-posn-x seg)
  (posn-x seg))


; worm-head-y : Worm -> Coordinate
; Returns the y position of the center of the worm's lead segment.
; structural decompostiion on worm
(define (worm-head-y wm)
  (get-y (worm-body wm)))

; get-y: Body -> Integer
; RETURN the y position of the center of the worm's lead segment
; Structural decomposition on body

(define (get-y bd)
  (get-posn-y (first bd)))

; get-posn-y: Posn -> Integer
; RETURN the y position of teh center of the worm's lead segment
; structural decomposition on posn
(define (get-posn-y seg)
  (posn-y seg))

; replace-worm : World Worm -> World
; Inserts the given Worm into the given World,
; replacing the existing Worm.
; WHERE: The Worm does not overlap with the food.

(define (replace-worm wld wm)
  (make-world (world-food wld)
              wm))

; test
(begin-for-test
  (check-equal? 
   (replace-worm 
    (make-world (make-posn 100 100)
                (make-worm (list (make-posn 50 80)
                                 (make-posn 60 80))
                           "left"))
    (make-worm (list (make-posn 40 80)
                     (make-posn 50 80))
               "left"))
   (make-world (make-posn 100 100)
               (make-worm (list (make-posn 40 80)
                                (make-posn 50 80))
                          "left"))))

; replace-food : World Posn -> World
; Inserts a piece of food into the world at the given Coordinates,
; replacing the existing food.
; WHERE: The food does not overlap with any of the worm's segments.
; Structural decomposition
(define (replace-food wld fd)
  (make-world fd (world-worm wld)))

; TEST
(begin-for-test
  (check-equal? 
   (replace-food 
    (make-world (make-posn 50 80)
                (make-worm (list (make-posn 50 80)
                                 (make-posn 60 80))
                           "left"))
    (make-posn 40 20))
   (make-world (make-posn 40 20)
               (make-worm (list (make-posn 50 80)
                                (make-posn 60 80))
                          "left"))))
; posns-overlap? : Posn ListOfPosn -> Boolean
; Returns true if p overlaps with any elements of ps.
; Two posns touching at only their outer edges are not overlapping.
; STRATEGY: structural decompsotion on List of Posn

(define (posns-overlap? p ps)
  (cond
    [(empty? ps) false]
    [(cons? ps) (or (check-overlap-two-posn p (first ps))
                    (posns-overlap? p (rest ps)))]))

; TEST
(begin-for-test
  (check-equal? (posns-overlap? (make-posn 40 70)
                                (list (make-posn 50 80)(make-posn 60 80)))
                false))

; check-overlap-two-posn: Posn Posn -> Boolean
; RETURN true if two posns are the same, false otherwise
; Structural decomposition

(define (check-overlap-two-posn p1 p2)
  (and (= (posn-x p1)(posn-x p2))
       (= (posn-y p1)(posn-y p2))))

; TEST
(begin-for-test
  (check-equal? (check-overlap-two-posn (make-posn 45 55)(make-posn 55 65))
                false)
  (check-equal? (check-overlap-two-posn (make-posn 45 55)(make-posn 45 55))
                true))

; 
; ============================ RANDOM FUNCTIONS ===============================

; random-posn : PosInteger PosInteger PosInteger PosInteger
; Returns a random posn within a width x height canvas.
; WHERE: the returned posn satisfies the randomization 
; as well as on the path where the worm can go through
; Functinal Composition
(define (random-posn width height interval offset)
  (make-posn
   (+ offset (* interval (random (quotient width interval))))
   (+ offset (* interval (random (quotient height interval))))))

; random-food : ListOfPosn -> Food
; RETURN a random allowed posn given a list of posn that 
; are not allowed to be teh position of the food
; Functional composition

(define (random-food not-allowed)
  (food-check
   (random-posn WIDTH HEIGHT FOOD-DIAMETER FOOD-RADIUS)
   not-allowed))

; Not able to test

; food-check : Food ListOfPosn -> Posn
; RETURN the right posn which is not overlapping with the body
; Strategy: generative recursion
(define (food-check candidate-food not-allowed)
  (if (posns-overlap? candidate-food not-allowed)
      (random-food not-allowed)
      candidate-food))

; TEST
(check-random (food-check (make-posn 45 65)
                          (list (make-posn 45 65)
                                (make-posn 55 65)))
              (random-food (list (make-posn 45 65)
                                 (make-posn 55 65))))


; INITIAL WORLD

(define INITIAL-FOOD (random-food 
                      (list (make-posn SEGMENT-RADIUS SEGMENT-RADIUS))))
(define INITIAL-WORM (make-worm 
                      (list (make-posn SEGMENT-RADIUS SEGMENT-RADIUS)) 
                      "down"))
(define INITIAL-WORLD (make-world INITIAL-FOOD INITIAL-WORM))


; ============================ KEY-HANDLER ===============================

; key-handler: World KeyEvent -> World
; RETURN a new World based on the input keyevent value
; WHERE
;   - "left"/"up"/"right"/"down" will make the worm turns 
;   to the corresponding different directions

(define (key-handler wld kev)
  (cond
    [(string=? kev "left") (change-direction-world wld "left")]
    [(string=? kev "right") (change-direction-world wld "right")]
    [(string=? kev "up") (change-direction-world wld "up")]
    [(string=? kev "down") (change-direction-world wld "down")]))

; TEST
(begin-for-test
  (check-equal? 
   (key-handler 
    (make-world (make-posn 45 55)
                (make-worm
                 (list (make-posn 5 215)
                       (make-posn 5 205))
                 "up")) "left")
   (make-world (make-posn 45 55)
               (make-worm
                (list (make-posn 5 215)
                      (make-posn 5 205))
                "left")))
  (check-equal? 
   (key-handler 
    (make-world (make-posn 45 55)
                (make-worm
                 (list (make-posn 5 215)
                       (make-posn 5 205))
                 "up")) "right")
   (make-world (make-posn 45 55)
               (make-worm
                (list (make-posn 5 215)
                      (make-posn 5 205))
                "right")))
  (check-equal? 
   (key-handler 
    (make-world (make-posn 45 55)
                (make-worm
                 (list (make-posn 5 215)
                       (make-posn 5 205))
                 "up")) "down")
   (make-world (make-posn 45 55)
               (make-worm
                (list (make-posn 5 215)
                      (make-posn 5 205))
                "down")))
  (check-equal? 
   (key-handler 
    (make-world (make-posn 45 55)
                (make-worm
                 (list (make-posn 5 215)
                       (make-posn 5 205))
                 "left")) "up")
   (make-world (make-posn 45 55)
               (make-worm
                (list (make-posn 5 215)
                      (make-posn 5 205))
                "up"))))

; change-direction-world: World String ->  World
; RETURN a new world where the direction of the worm 
; changed based on teh input keyevent
; Structural decomposition
(define (change-direction-world wld kev)
  (make-world (world-food wld)
              (change-direction-worm (world-worm wld) kev)))

; change-direction-worm: Worm KeyEvent -> Worm
; RETURN a new worm with the direction changed based
; on the given key event
; Structural decomposition
(define (change-direction-worm wm kev)
  (make-worm (worm-body wm)
             kev))

; ============================ BIG-BANG ===============================
; run: World NonNegReal -> World (Big-bang)

(define (run w tick-rate)
  (big-bang w
            (on-tick next-world tick-rate)
            (on-key key-handler)
            (on-draw render)
            (stop-when end? render-last)))


; ============================ END GAME ===============================

; end?: World -> Boolean
; RETURN true if the world is end conditions are met
; WHERE
;  - The worm hit teh wall
;  - run into itself
; STRATEGY: Strucutral decomposition on World

(define (end? wld)
  (or (hit-wall? (world-worm wld))
      (run-into-itself? (world-worm wld))))

; TEST
(begin-for-test
  (check-equal? 
   (end? (make-world
          (make-posn 35 55)
          (make-worm
           (list (make-posn 85 65) 
                 (make-posn 85 75) 
                 (make-posn 85 65)) "up")))
   true)
  (check-equal? 
   (end? (make-world
          (make-posn 35 55)
          (make-worm
           (list (make-posn 305 65) 
                 (make-posn 295 65) 
                 (make-posn 285 65)) "right")))
   true))

; hit-wall? : Worm -> Boolean
; return ture if next-world will make the x or y coordinate
; out of the canvas given the direction, false otherwise
; STRATEGY: functional composition
(define (hit-wall? wm)
  (hit-wall-new-worm wm))

; hit-wall-new-worm: Worm -> Boolean
; RETURN true if the worm is out of the canvas
; STRATEGY: Functional composition
(define (hit-wall-new-worm wm)
  (hit-wall-x-y (worm-head-x wm)
                (worm-head-y wm)))

; hit-wall-x-y: Integer Integer -> Boolean
; RETURN true if next-world will make the x or y 
; out of the canvas given the direction
; STRATEGY: Functional composition
(define (hit-wall-x-y x-coor y-coor)
  (or (< x-coor SEGMENT-RADIUS)
      (> x-coor (- WIDTH SEGMENT-RADIUS))
      (> y-coor (- HEIGHT SEGMENT-RADIUS))
      (< y-coor SEGMENT-RADIUS)))

; run-into-itself? : Worm -> Boolean
; RETURN true if the worm runs into itself
; false otherwise
; STRATEGY: Strucutral decomposition On worm
(define (run-into-itself? wm)
  (or (posns-overlap? (first (get-body wm)) 
                      (rest (get-body wm)))
      (inconsistency-btw-pos-dir (first (get-body wm)) 
                                 (rest (get-body wm))
                                 (worm-direction wm))))

; TEST
(begin-for-test 
  (check-equal? (run-into-itself? 
                 (make-worm 
                  (list (make-posn 30 60)) "right"))
                false)
  (check-equal? (run-into-itself?
                 (make-worm
                  (list (make-posn 85 65) 
                        (make-posn 85 75) 
                        (make-posn 85 65))"up")) 
                true)
  (check-equal? (run-into-itself? 
                 (make-worm
                  (list (make-posn 5 215)
                        (make-posn 5 205))
                  "up"))
                true))

; inconsistency-btw-pos-dir: Posn ListofPosns Direction -> Boolean
; RETURN true if there is inconsistancy between the direction and 
; the direction deducted from the first two segments. Aiming at dealing
; with the scenario WHERE
;  - the worm has only 2 segments and *flipping* won't but should 
; affect the game to be over
; STRATEGY:Structural decomposition on the Body


(define (inconsistency-btw-pos-dir head restbd dir)
  (cond 
    [(empty? restbd) false]
    [else (inconsistency-dir (get-direction-from-posn head (first restbd))
                             dir)]))

; TEST
(begin-for-test
  (check-equal? (inconsistency-btw-pos-dir 
                 (make-posn 215 185) (list (make-posn 215 175)) "up")
                true)
  (check-equal? (inconsistency-btw-pos-dir 
                 (make-posn 215 165) (list (make-posn 215 175)) "down")
                true)
  (check-equal? (inconsistency-btw-pos-dir 
                 (make-posn 185 215) (list (make-posn 175 215)) "left")
                true)
  (check-equal? (inconsistency-btw-pos-dir 
                 (make-posn 165 215) (list (make-posn 175 215)) "right")
                true))

; inconsistency-dir : Direction Direction -> Boolean
; RETURN true if the two directions are opposite; false
; otherwise
; Structural decomposition on direction

(define (inconsistency-dir curr-dir dir)
  (cond
    [(string=? dir "left") (string=? curr-dir "right")]
    [(string=? dir "right") (string=? curr-dir "left")]
    [(string=? dir "up") (string=? curr-dir "down")]
    [(string=? dir "down") (string=? curr-dir "up")]))

; get-direction-from-posn: Posn Posn -> Direction
; RETURN the direction based on the worm's position
; Structural decomposition on the posn

(define (get-direction-from-posn head second)
  (if (= (posn-x head)(posn-x second))
      (up-or-down head second)
      (left-or-right head second)))

; up-or-down: Posn Posn -> Direction
; RETURN "down" if the head's y is larger than second
; "up" otherwise
; Structural decomposition on Posn

(define (up-or-down head second)
  (if (> (posn-y head) (posn-y second))
      "down"
      "up"))

; left-or-right: Posn Posn -> Direction
; RETURN "right" if the head's x is larger than second
; "left" otherwise
; Structural decomposition on Posn
(define (left-or-right head second)
  (if (> (posn-x head) (posn-x second))
      "right"
      "left"))

; ============================ ALTHERNATIVE DATA DEF ===============================

; 1. Definition of world: mine is (make-world food worm)
; the alternative definition can include direction:
; WHERE
;   - (make-world food worm direction)  

; PROS:Easier to select the direction of the worm;
; having this one structure, the worm definition can 
; change to a list of posns instead of a strucuture

; CONS:Hard to modify the design of worm to accomodate future 
; designs - e.g:
;    add features to the worm such as it's speed, paused?, or
;    being dragged to different positions

; 2. Definition of the food, mine is a posn
; the alternative definition might be a structure
; WHERE
;   - (make-food posn)
; Pros:
;  open for future designs, such as the shape of the food, 
; may be a circle of a squre or a star
; Cons:
;  a little more complicated than just using a posn

; 3 worm definition, can use x and y to represent the position,
; mine is a posn.
; mine is making the selecting a little more complicated.









