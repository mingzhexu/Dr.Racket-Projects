;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname snake-in-class) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
                                                                                                    
(require 2htdp/image)
(require 2htdp/universe)

;                                                                                                                                                                  
;                                                                                                                                                                  
;                         ;                                                  ;;;       ;                   ;        ;          ;                                   
;   ;;;;;;                ;                           ;;;;;;                ;          ;                   ;        ;          ;                                   
;    ;    ;               ;                            ;    ;               ;                                       ;                                              
;    ;     ;   ;;;;;    ;;;;;;;    ;;;;;               ;     ;    ;;;     ;;;;;;     ;;;      ;; ;;;     ;;;      ;;;;;;;    ;;;        ;;;     ;; ;;;     ;;;;;;  
;    ;     ;        ;     ;             ;              ;     ;   ;   ;      ;          ;       ;;   ;      ;        ;          ;       ;   ;     ;;   ;   ;     ;  
;    ;     ;   ;;;;;;     ;        ;;;;;;              ;     ;  ;     ;     ;          ;       ;    ;      ;        ;          ;      ;     ;    ;    ;   ;        
;    ;     ;  ;     ;     ;       ;     ;              ;     ;  ;;;;;;;     ;          ;       ;    ;      ;        ;          ;      ;     ;    ;    ;    ;;;;;   
;    ;     ;  ;     ;     ;       ;     ;              ;     ;  ;           ;          ;       ;    ;      ;        ;          ;      ;     ;    ;    ;         ;  
;    ;    ;   ;    ;;     ;    ;  ;    ;;              ;    ;    ;    ;     ;          ;       ;    ;      ;        ;    ;     ;       ;   ;     ;    ;   ;     ;  
;   ;;;;;;     ;;;; ;;     ;;;;    ;;;; ;;            ;;;;;;      ;;;;    ;;;;;;    ;;;;;;;   ;;;  ;;;  ;;;;;;;      ;;;;   ;;;;;;;     ;;;     ;;;  ;;;  ;;;;;;   
;                                                                                                                                                                  
;                                                                                                                                                                  
;                                                                                                                                                                  
;                                                                                                                                                                  

;; A Food is a Posn

;; A Direction is one of 
;; - 'up
;; - 'down 
;; - 'left
;; - 'right


;; A Segments is Lof[Segment] 
;; A Segment is a Posn

;; A Snake is (make-snake Direction Segments) represents a snake 
;; with direction dir and body segments
(define-struct snake (dir segments))


;; A World is (make-world Snake Food) where
;; snake represents the snake in the game 
;; food represents the food in the game 
(define-struct world (snake food))


;                                                                                            
;                                                                                            
;                                             ;                             ;                
;     ;;;;;;                                  ;                             ;                
;    ;     ;                                  ;                             ;                
;   ;      ;    ;;;     ;; ;;;     ;;;;;;   ;;;;;;;    ;;;;;    ;; ;;;    ;;;;;;;    ;;;;;;  
;   ;          ;   ;     ;;   ;   ;     ;     ;             ;    ;;   ;     ;       ;     ;  
;   ;         ;     ;    ;    ;   ;           ;        ;;;;;;    ;    ;     ;       ;        
;   ;         ;     ;    ;    ;    ;;;;;      ;       ;     ;    ;    ;     ;        ;;;;;   
;   ;         ;     ;    ;    ;         ;     ;       ;     ;    ;    ;     ;             ;  
;    ;     ;   ;   ;     ;    ;   ;     ;     ;    ;  ;    ;;    ;    ;     ;    ;  ;     ;  
;     ;;;;;     ;;;     ;;;  ;;;  ;;;;;;       ;;;;    ;;;; ;;  ;;;  ;;;     ;;;;   ;;;;;;   
;                                                                                            
;                                                                                            
;                                                                                            
;                                                                                            


(define WIDTH 400) 

(define HEIGHT 400)

(define SEGMENT-SIDE 10)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define FOOD-IMAGE (square SEGMENT-SIDE 'solid 'green)) 

(define SEGMENT-IMAGE (square SEGMENT-SIDE 'solid 'red))

(define SNAKE-INIT (make-snake 'right (list (make-posn 10 10))))

(define FOOD-INIT (make-posn 200 200))

(define WORLD-INIT (make-world SNAKE-INIT FOOD-INIT))


;                                                                        
;                                                                        
;                                              ;                         
;   ;;;;;;                                     ;                         
;    ;    ;                                                              
;    ;     ;  ;;; ;;;;   ;;;;;   ;;;    ;;;  ;;;      ;; ;;;      ;;; ;; 
;    ;     ;    ;;            ;   ;  ;;  ;     ;       ;;   ;    ;   ;;  
;    ;     ;    ;        ;;;;;;   ;  ;;  ;     ;       ;    ;   ;     ;  
;    ;     ;    ;       ;     ;    ; ;; ;;     ;       ;    ;   ;     ;  
;    ;     ;    ;       ;     ;    ; ; ;;      ;       ;    ;   ;     ;  
;    ;    ;     ;       ;    ;;    ;;  ;;      ;       ;    ;    ;   ;;  
;   ;;;;;;    ;;;;;;     ;;;; ;;    ;  ;;   ;;;;;;;   ;;;  ;;;    ;;; ;  
;                                                                     ;  
;                                                                     ;  
;                                                                 ;;;;   
;                                                                        


;; draw-world : World -> Image 
;; Given a world represent it as an image 
(define (draw-world w)
  (draw-snake (world-snake w)
              (draw-food (world-food w) BACKGROUND)))


;; draw-snake : Snake Image -> Image 
;; Given a snake and an image draw the snake on the image and return the new image
(define (draw-snake s img)
  (draw-segments (snake-segments s) img))

;; draw-segments : Lof[Segment] Image -> Image 
;; Given a list of Segments draw it on image and return the new image
(define (draw-segments los img)
  (cond 
    [(empty? los) img]
    [else (place-image SEGMENT-IMAGE 
                       (posn-x (first los))
                       (posn-y (first los))
                       (draw-segments (rest los) img))]))

;; draw-food : Food Image -> Image 
;; Given a food and an image place the food on the image and return the new image 
(define (draw-food f img)
  (place-image FOOD-IMAGE 
               (posn-x f)
               (posn-y f)
               img))




;                                                                                                                
;                                                                                                                
;                         ;                                                 ;          ;                         
;   ;;;;;;;               ;                                                 ;          ;                         
;      ;                  ;                                                 ;                                    
;      ;      ;; ;;;    ;;;;;;;     ;;;     ;;; ;;;;   ;;;;;      ;;;;;   ;;;;;;;    ;;;        ;;;     ;; ;;;   
;      ;       ;;   ;     ;        ;   ;      ;;            ;    ;    ;     ;          ;       ;   ;     ;;   ;  
;      ;       ;    ;     ;       ;     ;     ;        ;;;;;;   ;           ;          ;      ;     ;    ;    ;  
;      ;       ;    ;     ;       ;;;;;;;     ;       ;     ;   ;           ;          ;      ;     ;    ;    ;  
;      ;       ;    ;     ;       ;           ;       ;     ;   ;           ;          ;      ;     ;    ;    ;  
;      ;       ;    ;     ;    ;   ;    ;     ;       ;    ;;    ;    ;     ;    ;     ;       ;   ;     ;    ;  
;   ;;;;;;;   ;;;  ;;;     ;;;;     ;;;;    ;;;;;;     ;;;; ;;    ;;;;       ;;;;   ;;;;;;;     ;;;     ;;;  ;;; 
;                                                                                                                
;                                                                                                                
;                                                                                                                
;                                                                                                                



;; world-step: World -> World 
;; Given the current world generate the next one and return it 
(define (world-step w)  
  (cond 
    [(eating? w) (eat&grow w)]
    [else (make-world (snake-move (world-snake w))
                      (world-food w))]))


;;eat&grow : World -> World
;; Given the current world return a new world where 
;; 1. The snake moves in the given direction
;; 2. The snakes increases in size by one segment 
;; 3. A new food appears in a new random location 
(define (eat&grow w)
  (make-world (snake-grow (world-snake w))
              (make-posn (* SEGMENT-SIDE (random (floor (/ WIDTH SEGMENT-SIDE))))
                         (* SEGMENT-SIDE (random (floor (/ HEIGHT SEGMENT-SIDE)))))))

;; snake-grow : Snake -> Snake 
;; Increase the snake's size by one segment. 
(define (snake-grow s)
  (make-snake (snake-dir s)
              (snake-move&grow (snake-segments s) (snake-dir s))))

;; snake-move&grow: Lof[Segment] Direction  -> Lof[Segment]
;; Add a new segment to the list in the appropriate direction. 
(define (snake-move&grow los dir)
  (cond 
    [(empty? los) los]
    [else (cons (create-new-head dir (first los))
                los)]))

;; eating? : World -> Boolean 
;; Given a world return true if the snake is about to consume a food else false
(define (eating? w)
  (segment-over-food (snake-segments (world-snake w)) (world-food w)))

;; segment-over-food : Lof[Segment] Food -> Boolean
;; True if the first of the list is over the food, false otherwise
(define (segment-over-food los food)
  (cond
    [(empty? los) false]
    [else (posn=? (first los) food)]))


;;posn=? : Posn Posn -> Boolean 
;;return true of the two posns have the same coordinates, false otherwise
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))


;; snake-move : Snake -> Snake 
;; Given a snake move it one segment distance in the correct direction 
(define (snake-move s)
  (make-snake (snake-dir s)
              (segments-move (snake-dir s) (snake-segments s))))

;; segments-move : Direction Lof[Segment] -> Lof[Segment]
;; move the segments in the direction given. Create a new segment in the direction and make it the head
;; then remove the last segment. 
(define (segments-move dir los)
  (cond 
    [(empty? los) los]
    [else (cons (create-new-head dir (first los))
                (remove-last-element los))]))


;; create-new-head : Direction Segment -> Segment
;; create a new segment in the direction of dir 
(define (create-new-head dir segment)
  (cond 
    [(symbol=? 'up dir) 
     (make-posn (posn-x segment) (- (posn-y segment) SEGMENT-SIDE))] 
    [(symbol=? 'down dir) 
     (make-posn (posn-x segment) (+ (posn-y segment) SEGMENT-SIDE))]
    [(symbol=? 'left dir)
     (make-posn (- (posn-x segment) SEGMENT-SIDE) (posn-y segment))]
    [(symbol=? 'right dir) 
     (make-posn (+ (posn-x segment) SEGMENT-SIDE) (posn-y segment))]))

;; remove-last-element : Lof[Segment] -> Lof[Segment]
;; given a list return the same list with the last element removed
(define (remove-last-element los)
  (cond 
    [(empty? los) los]
    [(empty? (rest los)) empty]
    [else (cons (first los) (remove-last-element (rest los)))]))




;; key-handler : World Key-Event -> World
;; Handle things when the user hits a key on the keyboard.
(define (key-handler w ke)
  (cond 
    [(or (key=? ke "up")
         (key=? ke "down")
         (key=? ke "left")
         (key=? ke "right"))
     (make-world (make-snake (string->symbol ke) (snake-segments (world-snake w)))
                 (world-food w))]
    [else w])) 




;                                                                                  
;                                                                                  
;                           ;;;                                                    
;   ;;;;;;;;                  ;                                                    
;    ;     ;                  ;                                                    
;    ;     ;  ;; ;;;      ;;; ;               ;;; ;;   ;;;;;   ;;;; ;;      ;;;    
;    ;  ;      ;;   ;    ;   ;;              ;   ;;         ;   ;  ;  ;    ;   ;   
;    ;;;;      ;    ;   ;     ;             ;     ;    ;;;;;;   ;  ;  ;   ;     ;  
;    ;  ;      ;    ;   ;     ;             ;     ;   ;     ;   ;  ;  ;   ;;;;;;;  
;    ;     ;   ;    ;   ;     ;             ;     ;   ;     ;   ;  ;  ;   ;        
;    ;     ;   ;    ;    ;   ;;              ;   ;;   ;    ;;   ;  ;  ;    ;    ;  
;   ;;;;;;;;  ;;;  ;;;    ;;; ;;              ;;; ;    ;;;; ;; ;;; ;; ;;    ;;;;   
;                                                 ;                                
;                                                 ;                                
;                                             ;;;;                                 
;                                                                                  


;; end-game? : World -> Boolean 
;; true if one of the condition that end the game has been met, false otherwise
(define (end-game? w)
  (or (out-of-bounds? (snake-segments (world-snake w)))
      (snake-collide-with-self (snake-segments (world-snake w)))))


;; snake-collide-with-self : Lof[Segment] -> Boolean 
;; true if the head collides with another segment, false otherwise
(define (snake-collide-with-self los)
  (cond 
    [(empty? los) false]
    [else (head-collides-with-tail? (first los) (rest los))]))

;; head-collides-with-tail? : Posn Lof[Posn] -> Boolean 
;; true if p has the same coordinates as one of the elements of lop, false otherwise 
(define (head-collides-with-tail? p lop)
  (cond
    [(empty? lop) false]
    [else (or (posn=? p (first lop))
              (head-collides-with-tail? p (rest lop)))]))

;; out-of-bounds? : Lof[Segment] -> Boolean 
;; Returns true if the head of the snake has gone out of bounds, false otherwise 
(define (out-of-bounds? los) 
  (cond 
    [(empty? los) false]
    [else (posn-out-of-bounds (first los))]))

;; posn-out-of-bounds : Posn -> Boolean 
;; true if the posn is out of bounds, false otherwise
(define (posn-out-of-bounds p)
  (or (<  (posn-x p) 0)
      (>= (posn-x p) WIDTH)
      (<  (posn-y p) 0)
      (>= (posn-y p) HEIGHT)))



;                                                                                  
;                                                                                  
;                ;                                                                 
;   ;;;;;;       ;                          ;;;;;;                                 
;    ;    ;                                  ;    ;                                
;    ;    ;    ;;;        ;;; ;;             ;    ;    ;;;;;    ;; ;;;      ;;; ;; 
;    ;    ;      ;       ;   ;;              ;    ;         ;    ;;   ;    ;   ;;  
;    ;;;;;       ;      ;     ;              ;;;;;     ;;;;;;    ;    ;   ;     ;  
;    ;    ;;     ;      ;     ;              ;    ;;  ;     ;    ;    ;   ;     ;  
;    ;     ;     ;      ;     ;              ;     ;  ;     ;    ;    ;   ;     ;  
;    ;     ;     ;       ;   ;;              ;     ;  ;    ;;    ;    ;    ;   ;;  
;   ;;;;;;;   ;;;;;;;     ;;; ;             ;;;;;;;    ;;;; ;;  ;;;  ;;;    ;;; ;  
;                             ;                                                 ;  
;                             ;                                                 ;  
;                         ;;;;                                              ;;;;   
;                                                                                  


(big-bang WORLD-INIT
          (to-draw draw-world)
          (on-tick world-step 0.15)
          (on-key key-handler)
          (stop-when end-game?))

