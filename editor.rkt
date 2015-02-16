;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "extras.rkt")
(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(define TIME-ON-TASK 5)

;(check-location "04" "editor.rkt")

(provide edit)
(provide string->editor)
(provide editor-pre)
(provide editor-post)
(provide editor-pos)

; =============================== CONSTANTS =============================== 
; constants 
(define HEIGHT 20) ; the height of the editor 
(define WIDTH 200) ; its width 
(define FONT-SIZE 16) ; the font size 
(define FONT-COLOR "black") ; the font color 

; graphical constants 
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

; =============================== DEFINITION =============================== 

; An Lo1S is one of: 
; – empty 
; – (cons 1String Lo1S)

; Editor: An Editor is (make-editor Lo1S Lo1S)
(define-struct editor (prev postp))

; WHERE
;  - prev is the part before the cursor
;  - postp is the part after the cursor

; Keyevent is one of
;   - space bar " "
;   - a single-character string, i.t., a string of length 1
;   - "left"
;   - "right"

#;(define (fn-keyevent ke)
    (cond
      [(string=? ke " ") ...]
      [(string=? ke "\b")...]
      [else ...]))


; =============================== DEFINITION FOR TEST =============================== 

(define EMPTY-EDITOR (make-editor '() '()))
(define EMPTY-PREV-EDITOR (make-editor '() (cons "M" (cons "A" (cons "G" empty)))))
(define EMPTY-POST-EDITOR (make-editor (cons "M" (cons "A" (cons "G" empty))) '()))
(define REGULAR-EDITOR 
  (make-editor (cons "I" (cons "m" empty))
               (cons "M" (cons "A" (cons "G" empty)))))
(define LONG-EDITOR (make-editor 
                     (list "a" "b" "c" "a" "b" "c" "a" 
                           "b" "c" "a" "b" "c" "a" "b"
                           "c" "a" "b" "c" "a" "b" "c"
                           "c" "a" "b" "c" "a" "b" "c"
                           "c" "a" "b" "c" "a" "b" "c")
                     '()))

; =============================== DRAWING =============================== 

; editor-render: Editor -> Image
; GIVEN an editor 
; RETURNs an editor on the empty scene
; Strategy: functional composition
(define (editor-render edi)
  (overlay/align "left" "center"
                 (editor-render-editor edi)
                 BACKGROUND))

; TEST
(begin-for-test
  (check-equal? 
   (editor-render EMPTY-EDITOR)
   (overlay/align "left" "center"
                  CURSOR
                  BACKGROUND)
   "given EMPTY editor, return only the cursor on background"))

; editor-render-editor: Editor -> Image
; GIVEN an editor, edi
; RETURN an image of the two texts separated by the cursor 
; STRATEGY: structural decomposition

(define (editor-render-editor edi)
  (beside (editor-render-prev (editor-prev edi))
          CURSOR
          (editor-render-postp (editor-postp edi))))

; editor-render-prev: List -> Image
; GIVEN a list of string and 
; RETURN the text image
; WHERE
;   - the list will first be reversed and then implode to string
; STRATEGY: Functional composition

(define (editor-render-prev prev-list)
  (text (implode (reverse prev-list)) FONT-SIZE FONT-COLOR))

; TEST
(begin-for-test 
  (check-equal? (editor-render-prev (list "c" "d"))
                (text "dc" FONT-SIZE FONT-COLOR)))

; editor-render-postp: List -> Image
; GIVEN a list of string and 
; RETURN the text image of the list
; STRATEGY: Functional composition

(define (editor-render-postp postp-list)
  (text (implode postp-list) FONT-SIZE FONT-COLOR))

; TEST
(begin-for-test 
  (check-equal? (editor-render-postp (list "c" "d"))
                (text "cd" FONT-SIZE FONT-COLOR)))

; list-to-image: Lo1S -> Image
; RETURN an image given a list of strings
; STRATEGY:functional composition
(define (list-to-image los)
  (text (implode los) FONT-SIZE FONT-COLOR))

; TEST
(begin-for-test 
  (check-equal? (list-to-image (list "c" "d"))
                (text "cd" FONT-SIZE FONT-COLOR)))



; =============================== INTERACTION =============================== 

; string->editor: String -> Editor
; Returns an Editor containing text str and cursor at position 0.
; STRATEGY:functional composition

(define (string->editor s)
  (make-editor '() (explode s)))

; TEST:
(begin-for-test
  (check-equal? (string->editor "abc")
                (make-editor '() (list "a" "b" "c"))
                "the prev part is store in reverse order")
  (check-equal? (string->editor " ")
                (make-editor '() (list " "))))

; edit: Editor String -> Editor
; Purpose: 
; given an editor e and an input string ke
; return a new editer based on the input string

; IF original text's LENGTH is longer than the background
;   - ignore the input 
;   - other keyevents such as delete or move cursor are not ignored
; STRATEGY:Functional Composition

(define (edit edi kev)
  (if (edit-length-okay edi) 
      (edit-modify edi kev)
      (no-input-edit edi kev)))

; TEST
(begin-for-test
  (check-equal? (edit (make-editor (list "A") (list "v")) "left")
                (make-editor '() (list "A" "v"))
                "cursor moving left regularly")
  (check-equal? (edit EMPTY-POST-EDITOR "left")
                (make-editor (list "A" "G") (list "M"))
                "cursor moving left get the prev's first element")
  (check-equal? (edit EMPTY-POST-EDITOR "right")
                EMPTY-POST-EDITOR
                "cursor moving right but no element on right side,
                return the input editor")
  (check-equal? (edit (make-editor 
                       (list "a" "b" "c" "a" "b" "c" "a" 
                             "b" "c" "a" "b" "c" "a" "b"
                             "c" "a" "b" "c" "a" "b" "c"
                             "c" "a" "b" "c" "a" "b" "c"
                             "c" "a" "b" "c" "a" "b" "c")
                       '()) "a")
                (make-editor 
                 (list "a" "b" "c" "a" "b" "c" "a" 
                       "b" "c" "a" "b" "c" "a" "b"
                       "c" "a" "b" "c" "a" "b" "c"
                       "c" "a" "b" "c" "a" "b" "c"
                       "c" "a" "b" "c" "a" "b" "c")
                 '())
                "out of range issue - no input allowed"))

; edit-length-okay: Editor -> Boolean
; RETURN true if the input is not longer than the canvas
; false otherwise, given the editor
; STRATEGY: structural decomposition

(define (edit-length-okay edi)
  (edit-length-okay-prev-postp (editor-prev edi)
                             (editor-postp edi)))

; TEST
(begin-for-test
  (check-equal? (edit-length-okay EMPTY-EDITOR) true)
  (check-equal? (edit-length-okay EMPTY-PREV-EDITOR) true)
  (check-equal? (edit-length-okay 
                 (make-editor 
                  (list "a" "b" "c" "a" "b" "c" "a" 
                        "b" "c" "a" "b" "c" "a" "b"
                        "c" "a" "b" "c" "a" "b" "c"
                        "c" "a" "b" "c" "a" "b" "c"
                        "c" "a" "b" "c" "a" "b" "c")
                  '())) false))
; edit-length-okay-prev-postp: Lo1S Lo1S -> Boolean
; RETURN a boolean value given two lists 
; strategy: 
(define (edit-length-okay-prev-postp los1 los2)
  (<= (+ (list-width los1)
         (list-width los2))
      WIDTH))

; TEST:
(begin-for-test
  (check-equal? (edit-length-okay-prev-postp (list "a")(list "b"))
                true)
  (check-equal? (edit-length-okay-prev-postp 
                 (list "a" "b" "c" "a" "b" "c" "a" 
                       "b" "c" "a" "b" "c" "a" "b"
                       "c" "a" "b" "c" "a" "b" "c"
                       "c" "a" "b" "c" "a" "b" "c"
                       "c" "a" "b" "c" "a" "b" "c")
                 '()) false))

; list-width: Lo1S -> NonNegReal
; RETURN the width of the list of strings if generated to an image
; STRATEGY:functional composition
(define (list-width los)
  (image-width (list-to-image los)))

; TEST
(begin-for-test
  (check-equal? (list-width (list "a" "a" "b"))
                27))




; =============================== EDIT-MODIFY =============================== 
; edit-modify: Editor String -> Editor
; RETURN a new editor given a current editor and the keyevent
; WHERE
;   - "left" "right" can move the cursor left/right
;   - "\t" or "\r" shouldn't affect the editor
;   - "\b" is to delete and delete 1string before the cursor
;   - all other one-letter input including " " will be added in editor
;   - for the other input, ignore
; STRATEGY: Structural decomposition

(define (edit-modify edi kev)
  (cond
    [(string=? kev "left") (move-left edi)]
    [(string=? kev "right")(move-right edi)]
    [(t-or-r? kev) edi]
    [(string=? kev "\b") (delete-one edi)]
    [(one-char kev) (attach edi kev)]
    [else edi]))

;TEST

(begin-for-test
  (check-equal? (edit-modify EMPTY-EDITOR "a")
                (make-editor (list "a" ) '()))
  (check-equal? (edit-modify EMPTY-EDITOR "\b")
                EMPTY-EDITOR)
  (check-equal? (edit-modify EMPTY-PREV-EDITOR "A")
                (make-editor (list "A" )
                             (list "M" "A" "G")))
  (check-equal? (edit-modify REGULAR-EDITOR "left")
                (make-editor (list "m" )
                             (list "I" "M" "A" "G")))
  (check-equal? (edit-modify REGULAR-EDITOR "right")
                (make-editor (list  "M" "I" "m" )
                             (list "A" "G")))
  (check-equal? (edit-modify REGULAR-EDITOR "\t")
                REGULAR-EDITOR)
  (check-equal? (edit-modify REGULAR-EDITOR "caps")
                REGULAR-EDITOR))

; move-left: Editor -> Editor
; RETURN A new editor with the prev part last 1string move to the postp 
; STRATEGY: Structural decomposition

(define (move-left edi) 
  (if (is-prev-empty? edi)
      edi
      (move-left-prev-postp (editor-prev edi)
                          (editor-postp edi))))

; TEST
(begin-for-test
  (check-equal? (move-left EMPTY-PREV-EDITOR)
                EMPTY-PREV-EDITOR))

; move-left-prev-postp: Lo1S Lo1S -> Editor
; RETURN an editor with the prev part dismiss *last* 1string
; and postp part concatenate that one given two lists.
; WHERE
;   - "Last" one means the first one since I store the 
;   prev-list in reversed order
; STRATEGY: structural decomposition

(define (move-left-prev-postp prev-list postp-list)
  (make-editor (rest prev-list)
               (cons (first prev-list) postp-list)))
; TEST
(begin-for-test
  (check-equal? (move-left-prev-postp (list "a" "b")
                                    (list "c" "d"))
                (make-editor (list "b")
                             (list "a" "c" "d"))
                "move first on in prev list to postp list"))

; move-right: Editor -> Editor
; RETURN A new editor with the postp part first 1string move to the prev 
; STRATEGY: Structural decomposition

(define (move-right edi)
  (if (is-postp-empty? edi)
      edi
      (move-right-prev-postp (editor-prev edi)
                           (editor-postp edi))))

; TEST
(begin-for-test
  (check-equal? (move-right EMPTY-POST-EDITOR)
                EMPTY-POST-EDITOR)
  (check-equal? (move-right EMPTY-PREV-EDITOR)
                (make-editor (list "M")
                             (list "A" "G"))))

; is-postp-empty?: Editor -> Boolean
; RETURN true if the editor postp part is empty 
; false otherwise
; STRATEGY: Structural decomposition

(define (is-postp-empty? edi)
  (empty? (editor-postp edi)))

; TEST
(begin-for-test
  (check-equal? (is-postp-empty? EMPTY-POST-EDITOR)
                true))

; move-right-prev-postp: Lo1S Lo1S -> Editor
; RETURN an editor with the postp part dismiss first 1string
; and prev part concatenate that onestring at *last* position.
; WHERE
;   - "Last" one means the first one since I store the 
;   prev-list in reversed order
; STRATEGY: structural decomposition

(define (move-right-prev-postp prev-list postp-list)
  
  (make-editor (cons (first postp-list) prev-list)
               (rest postp-list)))

; TEST
(begin-for-test 
  (check-equal? 
   (move-right-prev-postp (list "a" "c")(list "v" "b"))
   (make-editor (list "v" "a" "c")
                (list  "b"))))

; t-or-r? : String -> Boolean
; RETURN true if the string is t or r;
; STRATEGY:Functional composition
(define (t-or-r? kev)
  (or (string=? kev "\t")
      (string=? kev "\r")))

; delete-one: Editor -> Editor
; RETURN a new editor with one element removed from the prev-part
; STRATEGY:structural decomposition
(define (delete-one edi)
  (if (is-prev-empty? edi)
      edi
      (make-editor (delete-one-list (editor-prev edi))
                   (editor-postp edi))))


;TEST
(begin-for-test
  (check-equal? (delete-one REGULAR-EDITOR)
                (make-editor (list "m")
                             (list "M" "A" "G"))))

; is-prev-empty?: Editor -> Boolean
; RETURN true if the editor prev part is empty 
; false otherwise
; STRATEGY: Structural decomposition

(define (is-prev-empty? edi)
  (empty? (editor-prev edi)))

; delete-one-list: Lo1S -> Lo1S
; RETURN a new list of 1string with the first one removed
; STRATEGY:structrual decomposition
(define (delete-one-list lo1s)
  (rest lo1s))
; TEST
(begin-for-test
  (check-equal? 
   (delete-one-list (list "a" "v" "b"))
   (list "v" "b")))

; one-char: String -> Boolean
; RETURN true if the stirng is a one-character string
; STRATEGY:functional composition
(define (one-char kev)
  (= 1 (string-length kev)))

; attach : Editor String-> Editor
; RETURN a new editor with the given string attached
; STRATEGY: structural decomposition
(define (attach edi str)
  (make-editor (attach-list (editor-prev edi) str)
               (editor-postp edi)))

; attach-list: Lo1S String -> Lo1S
; RETURN a new Lo1S with the given string attached to front
; STRATEGY:functional composition
(define (attach-list lo1s str)
  (cons str lo1s))

; editor-pos: Editor -> Integer
; RETURN the position of the cursor
; strategy: Functional composition

(define (editor-pos edi)
  (string-length (implode (editor-prev edi))))

; TEST
(begin-for-test
  (check-equal? (editor-pos (make-editor 
                             (list "A" "b")
                             (list "c" "d")))
                2))

; =============================== NO INPUT EDIT =============================== 
; no-input-edit: Editor String -> Editor
; RETURN a new editor with the given editor and string, only 
; conduct delete and cursor move, other inputs are not taken
; GIVEN 
; STRATEGY:Strucural decomposition on the keyevent

(define (no-input-edit edi kev)
  (cond
    [(string=? kev "\b") (delete-one edi)]
    [(string=? kev "left")(move-left edi)]
    [(string=? kev "right")(move-right edi)]
    [else edi]))

; TEST
(begin-for-test
  (check-equal? (no-input-edit LONG-EDITOR "A")
                LONG-EDITOR)
  (check-equal? (no-input-edit LONG-EDITOR "left")
                (make-editor 
                     (list "b" "c" "a" "b" "c" "a" 
                           "b" "c" "a" "b" "c" "a" "b"
                           "c" "a" "b" "c" "a" "b" "c"
                           "c" "a" "b" "c" "a" "b" "c"
                           "c" "a" "b" "c" "a" "b" "c")
                     (list "a")))
  (check-equal? (no-input-edit LONG-EDITOR "\b")
                (make-editor 
                     (list "b" "c" "a" "b" "c" "a" 
                           "b" "c" "a" "b" "c" "a" "b"
                           "c" "a" "b" "c" "a" "b" "c"
                           "c" "a" "b" "c" "a" "b" "c"
                           "c" "a" "b" "c" "a" "b" "c")
                     '()))
  (check-equal? (no-input-edit LONG-EDITOR "right")
                (make-editor 
                     (list "a" "b" "c" "a" "b" "c" "a" 
                           "b" "c" "a" "b" "c" "a" "b"
                           "c" "a" "b" "c" "a" "b" "c"
                           "c" "a" "b" "c" "a" "b" "c"
                           "c" "a" "b" "c" "a" "b" "c")
                     '())))

; 


; editor-pre: Editor -> String
; RETURN a string as the prev part of the editor
; WHERE the order of characters are not reversed
; STRATEGY: structural decomposition

(define (editor-pre edi)
  (implode (reverse (editor-prev edi))))

; TEST
(begin-for-test
  (check-equal? 
   (editor-pre (make-editor (list "A" "b")
                            (list "C" "d")))
   "bA"))
   
; editor-post: Editor -> String
; RETURN a string as the post part of the editor
; GIVEN an Editor
; STRATEGY: structural decomposition

(define (editor-post edi)
  (implode (editor-postp edi)))

; TEST
(begin-for-test
  (check-equal? 
   (editor-post (make-editor (list "a" "b")
                             (list "v" "d")))
   "vd"))

; string-to-editor:
; Returns an Editor containing text str and cursor at right most position.
; Functional composition
(define (string-to-editor str)
  (make-editor (reverse (explode str))
               '()))

; TEST
(begin-for-test
  (check-equal?
   (string-to-editor "abc")
   (make-editor (list "c" "b" "a") '())))

; =============================== BIG-BANG =============================== 

; run : String -> Editor
; launches the editor given some initial string 
(define (run s)
  (big-bang (string-to-editor s)
            (on-key edit)
            (to-draw editor-render)))

; =============================== Alternative DATA DEF =============================== 
; 1:
; Editor can be represented by string instead of a list
; String operations can be easier 
; As what we did for the 2nd hw

; 2:
; Also the prev-part of the editor can be in order, instead of reversed;
; This may cause the edit a little bit complicated since everytime 
; editing the prev part, I need to reverse it first to get the last 
; element.

; However, this will cause the drawing easier to make. and The 
; initial world doesn't has to be reversed 






