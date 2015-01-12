;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Missile-Defense) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)
;;;; DATA DEFINITIONS 
;                                                                          
;                                                                          
;                                                                          
;                                                                          
;   ;;;;                                    ;;;;               ;;;         
;    ;  ;            ;                       ;  ;             ;            
;    ;   ;   ;;;;   ;;;;;    ;;;;            ;   ;   ;;;;   ;;;;;;         
;    ;   ;  ;    ;   ;      ;    ;           ;   ;  ;    ;    ;            
;    ;   ;   ;;;;;   ;       ;;;;;           ;   ;  ;;;;;;    ;            
;    ;   ;  ;    ;   ;      ;    ;           ;   ;  ;         ;            
;    ;  ;   ;   ;;   ;   ;  ;   ;;           ;  ;   ;         ;       ;;   
;   ;;;;     ;;; ;;   ;;;    ;;; ;;         ;;;;     ;;;;;  ;;;;;;    ;;   
;                                                                          
;                                                                          
;                                                                          
;                                                                          


;; A World is (make-world [Lof Posn] [Lof Posn] Defender)
;; interpretation: represents a world where 
;; bullets is the list of defender bullets in flight 
;; missiles is the list of missiles in flight
;; defender represents the defender   
(define-struct world (bullets missiles defender))  

;; A Defender is (make-defender Posn Direction Number)
;; interpretation: represents the defender with a location 
;; its direction and its current health
(define-struct defender (location direction health))

;; A Direction is one of 
;; - 'left
;; - 'right    


;;;; CONSTANTS 

;                                                                          
;                                                                          
;                                                                          
;                                                                          
;     ;;;;                                                                 
;    ;   ;                           ;                       ;             
;   ;        ;;;;   ;; ;;    ;;;;;  ;;;;;    ;;;;   ;; ;;   ;;;;;    ;;;;; 
;   ;       ;    ;   ;;  ;  ;    ;   ;      ;    ;   ;;  ;   ;      ;    ; 
;   ;       ;    ;   ;   ;   ;;;;    ;       ;;;;;   ;   ;   ;       ;;;;  
;   ;       ;    ;   ;   ;       ;   ;      ;    ;   ;   ;   ;           ; 
;    ;   ;  ;    ;   ;   ;  ;    ;   ;   ;  ;   ;;   ;   ;   ;   ;  ;    ; 
;     ;;;    ;;;;   ;;; ;;; ;;;;;     ;;;    ;;; ;; ;;; ;;;   ;;;   ;;;;;  
;                                                                          
;                                                                          
;                                                                          
;                                                                           

(define WIDTH 600)
(define HEIGHT 600)
(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define MISSILE-RADIUS 10)
(define MISSILE-DIAMETER (* 2 MISSILE-RADIUS))
(define MISSILE-SPEED 5)

(define BULLET-RADIUS 3)
(define BULLET-SPEED 5)

(define HEALTH-INIT 5)

(define DEFENDER-WIDTH 40)
(define DEFENDER-HEIGHT 10)
(define DEFENDER-SPEED 140)

(define DEFENDER-IMAGE 
  (rectangle DEFENDER-WIDTH DEFENDER-HEIGHT 'solid 'black))
(define DEFENDER-BULLET-IMAGE 
  (circle BULLET-RADIUS 'solid 'black))
(define MISSILE-IMAGE
  (circle MISSILE-RADIUS 'solid 'red))


;;;; Initial World 
(define DEFENDER-INIT 
  (make-defender (make-posn (floor (/ WIDTH 2))
                            (- HEIGHT DEFENDER-HEIGHT))
                 'left
                 HEALTH-INIT))

(define WORLD-INIT (make-world empty empty DEFENDER-INIT))

 
;for testing:
(define my-world1
  (make-world
   (list (make-posn 278 492))
   (list (make-posn 278 492))
   (make-defender (make-posn 300 500) 'left 5)))
(define my-world2 
  (make-world
   (list (make-posn 278 492) (make-posn 119 255) (make-posn 204 170) (make-posn 204 170))
   (list (make-posn 250 523) (make-posn 370 523))
   (make-defender (make-posn 373 590) 'right 5)))
(define my-world3
  (make-world
   (list (make-posn 105 25)
         (make-posn 85 5))
   (list (make-posn 20 590))
   (make-defender (make-posn 141 590) 'left 5)))
(define my-world4 
  (make-world
   (list (make-posn 245 405)
         (make-posn 269 381))
   (list (make-posn 400 237)
         (make-posn 460 237))
   (make-defender (make-posn 63 590) 'left 0)))

(define my-world5
  (make-world
   (list (make-posn 278 492))
   (list (make-posn 278 492)
         (make-posn 20 193)
         (make-posn 520 192)
         (make-posn 260 191)
         (make-posn 300 190)
         (make-posn 60 189))
   (make-defender (make-posn 300 500) 'left 5))) 
;                                                          
;                                                          
;                                                          
;                                                          
;   ;;;;                               ;                   
;    ;  ;                                                  
;    ;   ;  ;; ;;;   ;;;;   ;;; ;;;  ;;;    ;; ;;    ;;; ;;
;    ;   ;   ;;     ;    ;   ;   ;     ;     ;;  ;  ;   ;; 
;    ;   ;   ;       ;;;;;   ; ; ;     ;     ;   ;  ;    ; 
;    ;   ;   ;      ;    ;   ; ; ;     ;     ;   ;  ;    ; 
;    ;  ;    ;      ;   ;;   ; ; ;     ;     ;   ;  ;   ;; 
;   ;;;;    ;;;;;    ;;; ;;   ; ;    ;;;;;  ;;; ;;;  ;;; ; 
;                                                        ; 
;                                                    ;;;;  
;                                                          
;draw-world: World -> Image
;Given a world represent it as an image
(define (draw-world w)
  (draw-bullets (world-bullets w)
                (draw-missiles (world-missiles w)
                               (draw-defender (world-defender w)
                                              (draw-health (defender-health (world-defender w))
                                              BACKGROUND)))))


;TEST
(check-expect (draw-world my-world1)
              (draw-bullets (world-bullets my-world1)
                (draw-missiles (world-missiles my-world1)
                               (draw-defender (world-defender my-world1)
                                              (draw-health (defender-health (world-defender my-world1))
                                              BACKGROUND)))))

;draw-bullets: [Lof Posn] Image -> Image
;given a bullets (list of Posns),lob, and an image draw the bullets on the image
; and return the new image
(define (draw-bullets lob img)
  (cond
    [(empty? lob)img]
    [else (place-image DEFENDER-BULLET-IMAGE
                       (posn-x (first lob))
                       (posn-y (first lob))
                       (draw-bullets (rest lob) img))]))
;test 

(check-expect (draw-bullets (list ) BACKGROUND) BACKGROUND)
(check-expect (draw-bullets 
               (list (make-posn 122 480)
                     (make-posn 139 463))
                     BACKGROUND)
              (place-image DEFENDER-BULLET-IMAGE 
                           122 480
                           (place-image DEFENDER-BULLET-IMAGE 
                                        139 463
                                        BACKGROUND)))

;draw-missiles: [Lof Posn] Image -> Image
;given missiles (list of Posns, lom, and an image draw the bullets on the image
;and return the new image

(define (draw-missiles lom img)
  (cond
    [(empty? lom) img]
    [else (place-image MISSILE-IMAGE
                       (posn-x (first lom))
                       (posn-y (first lom))
                       (draw-missiles (rest lom) img))]))

;TEST:
(check-expect (draw-missiles (list ) BACKGROUND) BACKGROUND)
(check-expect (draw-missiles 
              (list (make-posn 122 480)
                    (make-posn 139 463))
                    BACKGROUND)
              (place-image MISSILE-IMAGE 122 480
                            (place-image MISSILE-IMAGE 
                                         139 463
                                         BACKGROUND)))

;draw-defender: Defender Image -> Image
;given a Defender d and an image, draw the defender on the image

(define (draw-defender d img)
  (place-image DEFENDER-IMAGE
               (posn-x (defender-location d))
               (posn-y (defender-location d)) 
               img))

;TEST
(check-expect (draw-defender (world-defender my-world1) BACKGROUND)
              (place-image DEFENDER-IMAGE
                           300 500
                           BACKGROUND))
                           
;draw-health: Number Image -> Image
;given a string health and an image, draw the health on teh image
(define (draw-health h img)
  (place-image (text 
                (string-append "Health: " 
                               (number->string h))
                20 "indigo")
               (/ WIDTH 2) 25
               img))

;test
(check-expect (draw-health 5 BACKGROUND)
              (place-image (text 
                            (string-append "Health: "
                                           (number->string 5))
                            20 "indigo")
                           (/ WIDTH 2) 25
                           BACKGROUND))
                           
                                                
;
;                                                                                          
;                                                                                          
;                                                                                          
;                                                                                          
;    ;;;;;                                                             ;                   
;      ;             ;                                       ;                             
;      ;    ;; ;;   ;;;;;    ;;;;   ;; ;;;   ;;;;    ;;; ;  ;;;;;    ;;;     ;;;;   ;; ;;  
;      ;     ;;  ;   ;      ;    ;   ;;     ;    ;  ;   ;;   ;         ;    ;    ;   ;;  ; 
;      ;     ;   ;   ;      ;;;;;;   ;       ;;;;;  ;        ;         ;    ;    ;   ;   ; 
;      ;     ;   ;   ;      ;        ;      ;    ;  ;        ;         ;    ;    ;   ;   ; 
;      ;     ;   ;   ;   ;  ;        ;      ;   ;;  ;    ;   ;   ;     ;    ;    ;   ;   ; 
;    ;;;;;  ;;; ;;;   ;;;    ;;;;;  ;;;;;    ;;; ;;  ;;;;     ;;;    ;;;;;   ;;;;   ;;; ;;;
;                                                                                          
;                                                                                          
;                                                                                          
;                                                                                          


;world-step: World -> World 
;Given the current world generate the next one and return it
;four conditions:
;- the total missiles are less than a fixed ammount -> generate new missiles
;- a missile reach bottom -> health - 1
;- missile and bullets hit -> need to disappear
;- missile and bullets out of bound -> need to disappear
;- for else -> just move

(define (world-step w)
  (cond
    [(less-than? (world-missiles w))
     (make-world (bullet-move (world-bullets w))
                 (missile-move (append (world-missiles w)
                                       (add-missile 1)))
                 (defender-move (world-defender w)))] 
    [(reach-bottom? (world-missiles w))(dehealth-move w)]
    [(hit? (world-bullets w)(world-missiles w))(both-disappear w)]
    [(out-of-bound? (world-bullets w)
                    (world-missiles w))(delete-missiles-bullets w)]
    [else (make-world (bullet-move (world-bullets w))
                      (missile-move (world-missiles w))
                      (defender-move (world-defender w)))]))  

;test
(check-random (world-step my-world1)
              (make-world 
               (bullet-move (list (make-posn 278 492)))
               (missile-move (append (list (make-posn 278 492))
                                     (list (make-posn 
                                            (* MISSILE-DIAMETER 
                                               (random (floor (/ WIDTH MISSILE-DIAMETER))))0)))) 
               (make-defender (make-posn 299 500) 'left 5)))

              
(check-random (world-step my-world2)
              (make-world
               (bullet-move (list (make-posn 278 492)
                                  (make-posn 119 255)
                                  (make-posn 204 170)
                                  (make-posn 204 170)))
               (missile-move (append (list (make-posn 250 523)
                                           (make-posn 370 523))
                                     (list (make-posn 
                                            (* MISSILE-DIAMETER 
                                               (random (floor (/ WIDTH MISSILE-DIAMETER))))0))))
               (make-defender (make-posn 374 590) 'right 5)))

(check-random (world-step my-world5)
              (make-world
                  empty
                  (missile-move
                   (list
                   (make-posn 20 193)
                   (make-posn 520 192)
                   (make-posn 260 191)
                   (make-posn 300 190)
                   (make-posn 60 189)))
                  (make-defender (make-posn 299 500) 'left 5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;less-than?: [Lof Posn] -> boolean
;given a list of posns, lom, return true if the number of 
;elements is less or equal to 5, false otherwise
(define (less-than? lom)
    (<= (num lom) 5))
;test
(check-expect (less-than?
               (list (make-posn 320 591)
                     (make-posn 400 591))) true)
               
;num: [Lof Posn] -> Number
(define (num lom)
  (cond
    [(empty? lom) 0]
    [(cons? lom) (+ 1 (num (rest lom)))]))
;test
(check-expect (num (list (make-posn 320 591)
                         (make-posn 400 591))) 2)




;add-missile: Number -> [Lof Posn]
;given the NUMBER OF missiles n to be generated, return 
;a list of Posns within the canvas and on the top of the canvas 
(define (add-missile n) 
  (cond
    [(= 0 n) empty]
    [else (cons 
           (make-posn (* MISSILE-DIAMETER (random (floor (/ WIDTH MISSILE-DIAMETER))))
           0)
           (add-missile (sub1 n)))]))

(check-random (add-missile 3)
              (list 
               (make-posn (* MISSILE-DIAMETER (random (floor (/ WIDTH MISSILE-DIAMETER)))) 0)
               (make-posn (* MISSILE-DIAMETER (random (floor (/ WIDTH MISSILE-DIAMETER)))) 0)
               (make-posn (* MISSILE-DIAMETER (random (floor (/ WIDTH MISSILE-DIAMETER)))) 0)))
           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;out-of-bound?: [Lof Posn] [Lof Posn] -> Boolean
; given a list of bullets and list of missiles and check
; if there are bullets or missiles out of bound
(define (out-of-bound? lob lom)
  (or (bullet-out? lob)
      (missile-out? lom)))
;test
(check-expect (out-of-bound? 
               (list (make-posn 350 68) (make-posn 160 -10))
               (list (make-posn 100 30) (make-posn 400 650)))
              true)
(check-expect (out-of-bound? (list )(list )) false)
;;bullet-out?: [Lof Posn] -> Boolean
; return true if there are bullets out of bound
(define (bullet-out? lob)
  (cond
    [(empty? lob) false]
    [(cons? lob)(or (not (bullet-inside (first lob)))
                    (bullet-out? (rest lob)))]))
;test
(check-expect (bullet-out? (list )) false)
(check-expect (bullet-out? 
               (list 
                (make-posn 350 68)
                (make-posn 186 537)
                (make-posn 160 -10)))true)
;bullet-inside: Posn -> Boolean
;given a bullet and return true if it is inside the scene
(define (bullet-inside b)
  (>= (posn-y b) (- 0 BULLET-RADIUS)))
;test
(check-expect (bullet-inside (make-posn 40 300)) true)
(check-expect (bullet-inside (make-posn 300 -10))false)
;missile-out: [Lop Posn] -> Boolean
;return true if there is any missile out of bound
(define (missile-out? lom)
  (cond
    [(empty? lom) false]
    [(cons? lom) (or (not (missile-inside (first lom)))
                     (missile-out? (rest lom)))]))
;test
(check-expect (missile-out? (list )) false)
(check-expect (missile-out? 
               (list (make-posn 220 132) (make-posn 310 132)))
               false)
(check-expect (missile-out? 
               (list (make-posn 30 650)))true)
;missile-inside: Posn -> Boolean
;given a missile and return true if it is inside the scene
(define (missile-inside m)
  (< (posn-y m) (+ HEIGHT MISSILE-RADIUS)))
;test
(check-expect (missile-inside (make-posn 220 132)) true)
(check-expect (missile-inside (make-posn 300 650)) false)
;delete-missiles-bullets: World -> World
; given a world return a new one with out-of-bound
; bullets and missiles deleted
(define (delete-missiles-bullets w)
  (make-world (bullet-move (delete-bullets (world-bullets w)))
              (missile-move (delete-missiles (world-missiles w)))
              (defender-move (world-defender w))))
;;test
(check-expect (delete-missiles-bullets my-world1)
              (make-world (list (make-posn 278 491))
                          (list (make-posn 278 493))
                          (make-defender 
                           (make-posn 299 500)
                           'left 5)))
(check-expect (delete-missiles-bullets my-world2)
              (make-world
               (list (make-posn 278 491) (make-posn 119 254)
                     (make-posn 204 169) (make-posn 204 169))
               (list (make-posn 250 524) (make-posn 370 524))
               (make-defender (make-posn 374 590) 'right 5)))
;delete-bullets: [Lof Posn] ->  [Lof Posn]
;given a list of bullets, delete those out of bound
(define (delete-bullets lob)
  (cond
    [(empty? lob) empty]
    [(bullet-inside (first lob))
     (cons (first lob)(delete-bullets (rest lob)))]
    [else (delete-bullets (rest lob))]))
;test
(check-expect (delete-bullets 
               (list (make-posn 410 97) (make-posn 150 97)))
              (list (make-posn 410 97) (make-posn 150 97)))
(check-expect (delete-bullets (list ))(list ))
(check-expect (delete-bullets (list (make-posn 410 97) (make-posn 150 -20)))
              (list (make-posn 410 97)))

;delete-missiles: [Lof Posn] ->  [Lof Posn]
;given a list of missiles, delete those that are out of bound
;and return a new list of missiles
(define (delete-missiles lom)
  (cond
    [(empty? lom) empty]
    [(missile-inside (first lom))
     (cons (first lom)(delete-missiles (rest lom)))]
    [else (delete-missiles (rest lom))]))
;test
(check-expect (delete-missiles (list )) (list ))
(check-expect (delete-missiles 
                (list (make-posn 410 97) (make-posn 150 97)))
              (list (make-posn 410 97) (make-posn 150 97)))
(check-expect (delete-missiles 
               (list (make-posn 410 97) (make-posn 150 620)))
              (list (make-posn 410 97)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;hit?: [Lop Posn] [Lop Posn] -> Boolean
;given world a list of bullets, lob, and a list of missiles , lom, true if 
;there are bullets touching missiles.
(define (hit? lob lom)
  (cond
    [(empty? lob) false]
    [(cons? lob) (or (bullet-single-hit? (first lob) lom)
              (hit? (rest lob) lom))]))
;test:
(check-expect (hit? (world-bullets my-world1)
                    (world-missiles my-world1)) true)
(check-expect (hit? (world-bullets my-world2)
                    (world-missiles my-world2)) false)

; bullet-single-hit?: Posn [Lof Posn] -> Boolean
; check if the bullet Posn b is touching any of the posns 
; as of missiles in the [Lof Posn], true if touched, false
; otherwise.
(define (bullet-single-hit? b lom)
  (cond
    [(empty? lom) false]
    [(cons? lom)(or (touch-missile? b (first lom))
                    (bullet-single-hit? b (rest lom)))]))
;test
(check-expect (bullet-single-hit? 
               (make-posn 450 105)
               (list (make-posn 450 105)(make-posn 210 105)))true)
(check-expect (bullet-single-hit? 
               (make-posn 150 105)
               (list (make-posn 450 105)(make-posn 210 105)))false)

;touch-missile?: Posn Posn -> Boolean
;given a posn(bullet) and a posn (missile) return true if they collide
(define (touch-missile? b m)
  (> (+ BULLET-RADIUS MISSILE-RADIUS)(distance b m)))
;test
(check-expect (touch-missile? (make-posn 150 105)(make-posn 450 105)) false)
(check-expect (touch-missile? (make-posn 150 105)(make-posn 210 105)) false)

;distance: Posn Posn -> Number
;given two posns, one is the center of the bullet circle, the other
;is the missile's, return distance between the two centers.
(define (distance p1 p2)
  (sqrt (+ (sqr (- (posn-x p1)(posn-x p2)))
           (sqr (- (posn-y p1)(posn-y p2))))))
;test
(check-expect (distance (make-posn 150 105)(make-posn 210 105))60)
(check-expect (distance (make-posn 150 105)(make-posn 450 105))300)

;both-disappear: World -> World
;given a world and return a new world with
;both bullets and missiles that are colliding remove from list
;others just move.
(define (both-disappear w)
  (make-world (bullet-move (update-bullets (world-bullets w)
                                           (world-missiles w)))
              (missile-move (update-missiles (world-bullets w)
                                             (world-missiles w)))
              (defender-move (world-defender w))))

(check-random (both-disappear my-world1)
              (make-world empty
                          empty
                          (make-defender (make-posn 299 500) 'left 5)))

(check-random (both-disappear my-world2)
              (make-world
                (list (make-posn 278 491)
                      (make-posn 119 254)
                      (make-posn 204 169)
                      (make-posn 204 169))
                (list (make-posn 250 524)
                      (make-posn 370 524))
                (make-defender (make-posn 374 590) 'right 5)))

;update-bullets: [Lof Posn] [Lof Posn] -> [Lof Posn]
;given a list of bullets and missiles, return a new list 
;of bullets with hitting bullets removed from the list
(define (update-bullets lob lom)
  (cond
    [(empty? lob) empty]
    [(bullet-single-hit? (first lob) lom)
     (update-bullets (rest lob) lom)]
    [else (cons (first lob)
                (update-bullets (rest lob) lom))]))
;test 
(check-expect (update-bullets (list )(list ))(list ))
(check-expect (update-bullets (list (make-posn 374 590))(list ))
              (list (make-posn 374 590)))
(check-expect (update-bullets (list (make-posn 374 590)
                                    (make-posn 260 73)
                                    (make-posn 20 73))
                              (list (make-posn 263 74)
                                    (make-posn 66 273)))
              (list (make-posn 374 590)(make-posn 20 73)))

; bullet-move: [Lof Posn] -> [Lof Posn]
; given a list of bullets, update location and return a new
; list of bullets

(define (bullet-move lob)
  (cond
    [(empty? lob) empty]
    [(cons? lob)(cons (one-bullet-move (first lob))
                      (bullet-move (rest lob)))]))

;;one-bullet-move: Posn ->  Posn
;given a bullet and return a new bullet moved up 
(define (one-bullet-move b)
  (make-posn (posn-x b)
             (- (posn-y b) BULLET-SPEED)))


; missile-move: [Lof Posn] -> [Lof Posn]
; given a list of missiles, update location and return a new
; list of missiles
(define (missile-move lom)
  (cond
    [(empty? lom) empty]
    [(cons? lom)(cons (one-missile-move (first lom))
                      (missile-move (rest lom)))]))

;one-missile-move: Posn -> Posn
;given a missile, return a new missile moving down ward
(define (one-missile-move m)
  (make-posn (posn-x m)
             (+ (posn-y m) MISSILE-SPEED)))

;update-missiles:[Lof Posn] [Lof Posn] -> [Lof Posn]
;given a list ofbullets and missiles, return a new list 
;of missiles dismissing all missiles being hit

(define (update-missiles lob lom)
  (cond
    [(empty? lom) empty]
    [(missile-single-hit? lob (first lom))
     (update-missiles lob (rest lom))]
    [else (cons (first lom)
                (update-missiles lob (rest lom)))]))

;test
(check-expect (update-missiles empty empty) empty)
(check-expect (update-missiles empty (list (make-posn 240 47)))
              (list (make-posn 240 47)))
(check-expect (update-missiles (list (make-posn 240 47)) empty) empty)
(check-expect (update-missiles (list (make-posn 240 47))
                               (list (make-posn 278 492) (make-posn 119 255)
                                     (make-posn 204 170) (make-posn 240 40)))
              (list (make-posn 278 492) (make-posn 119 255) (make-posn 204 170)))
;missile-single-hit?: [Lof Posn] Posn -> Boolean
;given a list of bullets and a missile, return true if the 
;missile being hit, false otherwise
(define (missile-single-hit? lob m)
  (cond
    [(empty? lob) false]
    [(cons? lob) (or (touch-missile? (first lob) m)
                     (missile-single-hit? (rest lob) m))]))
;test
(check-expect (missile-single-hit? empty (make-posn 300 29)) false)
(check-expect (missile-single-hit? 
               (list (make-posn 191 590) (make-posn 590 35)(make-posn 500 35))
               (make-posn 500 36)) true)

;defender-move: Defender -> Defender
;given a defender and move it based on its direction
;return a new defender with new position, note that the 
;defender can't move if it is moving left while atthe
;left corner; or moving right while at the right
;corner. 

(define (defender-move d)
  (cond
    [(and (symbol=? 'left (defender-direction d))
          (> (posn-x (defender-location d)) 0))
     (make-defender (location-left (defender-location d))
                    'left
                    (defender-health d))]
    [(and (symbol=? 'right (defender-direction d))
          (< (posn-x (defender-location d)) WIDTH))
     (make-defender (location-right (defender-location d))
                    'right
                    (defender-health d))]
    [else d]))

;test
(check-expect (defender-move (world-defender my-world1))
              (make-defender (make-posn 299 500) 'left 5))
(check-expect (defender-move 
                (make-defender (make-posn 0 500)'left 5))
                (make-defender (make-posn 0 500)'left 5))

;location-left: Posn -> Posn
;given a current location of the defender,
;return a new location moving to left
(define (location-left lol)
  (make-posn (- (posn-x lol) DEFENDER-SPEED)
             (posn-y lol)))

;location-right: Posn -> Posn
;given a current location of the defender,
;return a new location moving to right
(define (location-right lor)
  (make-posn (+ (posn-x lor) DEFENDER-SPEED)
             (posn-y lor)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;reach-bottom?: [Lof Posn] -> Boolean
; given a list of missiles, check if some of them are out of bound
(define (reach-bottom? lom)
  (cond
    [(empty? lom) false]
    [(cons? lom)(or (one-reach-bottom (first lom))
                    (reach-bottom? (rest lom)))]))
;test
(check-expect (reach-bottom? empty) false)
(check-expect (reach-bottom? 
               (list (make-posn 170 64)
                     (make-posn 260 67)
                     (make-posn 290 590))) true)

;one-reach-bottom: Posn -> Boolean
;true if the missile reaches the bound
(define (one-reach-bottom m)
  (>= (posn-y m) (- HEIGHT MISSILE-RADIUS)))
;test
(check-expect (one-reach-bottom (make-posn 290 590)) true)

;dehealth-move: World -> World
;given a world and return a new one wtih
;the health of defender changed 
(define (dehealth-move w)
  (make-world (bullet-move (world-bullets w))
              (missile-move (world-missiles w))
              (defender-move (update-defender 
                              (world-defender w)
                              (world-missiles w)))))
;test
(check-expect (dehealth-move my-world1)
              (make-world
                (list (make-posn 278 491))
                (list (make-posn 278 493))
                (make-defender (make-posn 299 500) 'left 5)))
(check-expect (dehealth-move my-world3)
              (make-world 
               (list (make-posn 105 24)
                     (make-posn 85 4))
               (list (make-posn 20 591))
   (make-defender (make-posn 140 590) 'left 4)))

;update-defender: Defender [Lof Posn] -> Defender
; given a defender and a list of missiles, update the defender's 
; health based on how many missiles touched the ground 

(define (update-defender d lom)
    (make-defender (defender-location d)
                   (defender-direction d)
                   (- (defender-health d)
                   (count-missile-reach lom))))
;test
(check-expect (update-defender
               (make-defender (make-posn 141 590) 'left 5) empty)
              (make-defender (make-posn 141 590) 'left 5))
(check-expect (update-defender 
               (make-defender (make-posn 141 590) 'left 5) 
               (list  (make-posn 259 26)
                      (make-posn 243 10)
                      (make-posn 140 590)))
              (make-defender (make-posn 141 590) 'left 4))
              
; count-missile-reach: [Lof Posn] -> Number
; given a list of missiles, count how many of them are 
; hitting the bottom edge of the scene and return the 
; number

(define (count-missile-reach lom)
  (cond
    [(empty? lom) 0]
    [(one-reach-bottom (first lom))
     (+ 1 (count-missile-reach (rest lom)))]
    [else (count-missile-reach (rest lom))]))
;test
(check-expect (count-missile-reach empty) 0)
(check-expect (count-missile-reach 
               (list (make-posn 400 92) (make-posn 140 590))) 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;key-handler: World Key-Event -> World
; handle things when teh user hits a key on the keyboard.
(define (key-handler w ke)
  (cond
    [(or (key=? ke "left")
         (key=? ke "right"))
     (make-world (world-bullets w)
                 (world-missiles w)
                 (make-defender 
                  (defender-location (world-defender w))
                  (string->symbol ke)
                  (defender-health (world-defender w))))]
    [(key=? ke " ") 
     (make-world (add-bullet (defender-location (world-defender w))
                             (world-bullets w))
                 (world-missiles w) 
                 (world-defender w))]
    [else w]))
;test
(check-expect (key-handler my-world1 "right")
              (make-world
               (list (make-posn 278 492))
               (list (make-posn 278 492))
              (make-defender (make-posn 300 500) 'right 5)))

(check-expect (key-handler my-world2 "left")
              (make-world
               (list (make-posn 278 492) (make-posn 119 255) (make-posn 204 170) (make-posn 204 170))
               (list (make-posn 250 523) (make-posn 370 523))
              (make-defender (make-posn 373 590) 'left 5)))

(check-expect (key-handler my-world3 " ")
               (make-world
                (list (make-posn 141 587)
                      (make-posn 105 25)
                      (make-posn 85 5))
                (list (make-posn 20 590))
                (make-defender (make-posn 141 590) 'left 5)))

(check-expect (key-handler my-world4 "up")
              (make-world
               (list (make-posn 245 405)
                     (make-posn 269 381))
               (list (make-posn 400 237)
                     (make-posn 460 237))
               (make-defender (make-posn 63 590) 'left 0)))
              
; add-bullet: Posn [Lof Posn]-> [Lof Posn]
; given a defender's posn dp and a list of bullets lob, 
; return new list of bullets with a new bullet added
; Since the hight of the defender is 10 we need to make 
; the starting point for the new bullet to be the height
; minus its radius 
(define (add-bullet dp lob)
  (cons (make-posn (posn-x dp)
                   (- (posn-y dp) BULLET-RADIUS))
        lob))  
(check-expect (add-bullet (make-posn 30 590) empty) (list (make-posn 30 587)))
(check-expect (add-bullet (make-posn 36 590) (list (make-posn 30 587)))
              (list (make-posn 36 587)(make-posn 30 587)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;                                                                  
;                                                                  
;                                                                  
;                                                                  
;   ;;;;;;              ;;            ;;;;                         
;    ;   ;               ;           ;   ;                         
;    ; ;    ;; ;;    ;;; ;          ;        ;;;;  ;; ;  ;   ;;;;  
;    ;;;     ;;  ;  ;   ;;          ;       ;    ;  ;; ;; ; ;    ; 
;    ; ;     ;   ;  ;    ;          ;   ;;;  ;;;;;  ;  ;  ; ;;;;;; 
;    ;       ;   ;  ;    ;          ;    ;  ;    ;  ;  ;  ; ;      
;    ;   ;   ;   ;  ;   ;;           ;   ;  ;   ;;  ;  ;  ; ;      
;   ;;;;;;  ;;; ;;;  ;;; ;;           ;;;    ;;; ;;;;; ;; ;  ;;;;; 
;                                                                  
;                                                                  
;                                                                  
;
; end-game? : World -> Boolean
; true if one of the conditions that end the game has been met, 
; false otherwise

(define (end-game? w)
  (no-health? (world-defender w)))

(check-expect (end-game? my-world3) false)
(check-expect (end-game? my-world4) true)
;no-health?: Defender -> Boolean
;true if the health of the defender is equal or less than 0;
(define (no-health? d)
  (<= (defender-health d) 0))
;test
(check-expect (no-health?
               (make-defender (make-posn 63 590) 'left 5))false)
(check-expect (no-health?
               (make-defender (make-posn 63 590) 'left 0))true)


;                                                                  
;                                                                  
;                                                                  
;                                                                  
;   ;;;;;      ;                    ;;;;;                          
;    ;   ;                           ;   ;                         
;    ;   ;   ;;;     ;;; ;;          ;   ;   ;;;;   ;; ;;    ;;; ;;
;    ;;;;      ;    ;   ;;           ;;;;   ;    ;   ;;  ;  ;   ;; 
;    ;   ;     ;    ;    ;           ;   ;   ;;;;;   ;   ;  ;    ; 
;    ;   ;     ;    ;    ;           ;   ;  ;    ;   ;   ;  ;    ; 
;    ;   ;     ;    ;   ;;           ;   ;  ;   ;;   ;   ;  ;   ;; 
;   ;;;;;    ;;;;;   ;;; ;          ;;;;;    ;;; ;; ;;; ;;;  ;;; ; 
;                        ;                                       ; 
;                    ;;;;                                    ;;;;  
;                                                                  
;                                                                  
(big-bang WORLD-INIT
          (to-draw draw-world)
          (on-tick world-step 0.01)
          (on-key key-handler)
          (stop-when end-game?))