;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname xexpr) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require 2htdp/batch-io)
(require 2htdp/image)
(require 2htdp/universe)

; ==================== CONSTANTS =====================
(define SCENE-WIDTH 300)
(define SCENE-HEIGHT 300)
(define BACKGROUND (empty-scene SCENE-WIDTH SCENE-HEIGHT))
(define CENTER-X (/ SCENE-WIDTH 2))
(define CENTER-Y (/ SCENE-HEIGHT 2))

(define PREFIX "https://en.wikipedia.org/wiki/")
(define INITIAL-ORDER false)
(define NO-IMAGE-INFO "NO PICTURE FOUND ON WIKIPEDIA")
(define FONT-SIZE 15)
(define FONT-COLOR "olive")
(define ONE-MORE-ROUND (text "ONE MORE ROUND!" FONT-SIZE FONT-COLOR))
(define MIN-IMG-HEIGHT 80) ;pixels

; ====================== DATA DEFINITION =========================
; An Xexpr is one of:
; - Symbol
; - String
; - Number
; - (cons Tag Sub-xexpr)

; A Tag is a Symbol, representing the name of an XML element.
(define H1 'h1)
(define P 'p)
(define BR 'br)

; A Sub-xexpr is one of 
; - ListOf<Xexpr> 
; - (cons ListOf<Attribute> ListOf<Xexpr>)

; A ListOf<X> is one of
; - empty
; - (cons X ListOf<X>)

; An Attribute is a (list AttrName AttrValue)
; representing an attribute name-value pair in an XML element.
; Example:
; An attribute which has href = "http://northeastern.edu" 
(define HREF-NEU (list 'href "http://northeastern.edu"))
; An attribute which has src = "ball.gif" 
(define IMG-SRC (list 'src "ball.gif"))

; An AttrName is a Symbol, 
; representing the name of an XML attribute.
; An AttrValue is a String, 
; representing the value associated with an XML attribute.

; A XexprOrEmpty is one of:
;  - '()
;  - Xexpr

; A URL is a string

; A World is a (make-world ListOf<URL> ListOf<URL>)
; WHERE
;  - The first list is the pre part of the image list, 
;    when the first image of the post list is displayed
;    that image will be stored to this pre list in the reversed 
;    order
;  - the second list is the post part of the image list,
;    where the initial input list of image will go. After displaying
;    one image that image will be removed from the post list
;    and added to the pre list.
(define-struct world (pre post))

; ====================== DEFINITION FOR TESTS =========================
; Represents an XML element.
(define HTML-EMPTY '(html ((lang "en-US")) (body (p) (br) (p) (br))))

; XML Element as a Symbol
(define XEXPR1 'level1)

; XML Element with a Sub-xexpr
(define XEXPR2
  (cons 'level2 
        (cons 
         (list
          (list 'height "10")
          (list 'width "20")
          (list 'center "100"))
         (list XEXPR1))))

; XML Element with a Sub-xexpr but no Attribute list
(define XEXPR3
  (cons 'level3
        (list XEXPR2)))

; HTML X-Expression with Text
(define HTML-WITH-TEXT
  '(html
    ((lang "en-US"))
    (body
     (p "Here is assignment " 5)
     (br)
     (p "This is a " (b "data") " definition."))))

; HTML Element with attribute.
(define HTML-ONLY-ATTR '(html ((lang "en-US"))))

; The Sub-xexpr of an empty HTML X-expression.
(define SUBXEXPR-HTML-EMPTY 
  '(((lang "en-US")) (body (p) (br) (p) (br))))

; The Sub-xexpr of HTML X-expression with text.
(define SUBXEXPR-HTML-WITH-TEXT
  '(((lang "en-US"))
    (body
     (p "Here is assignment " 5)
     (br)
     (p "This is a " (b "data") " definition."))))

;A list of Xexpr's
(define LIST-OF-XEXPR
  (list XEXPR1 XEXPR2 XEXPR3))

; Single Attribute
(define ATTRIBUTE1 '(lang "en-US"))
(define ATTRIBUTE2 '(B "date"))

; ListOf<Atrribute>
(define LIST-OF-ATTRI
  (list ATTRIBUTE1 ATTRIBUTE2))

(define IMG '(img ((src "balls.png") (width "100") (height "50"))))

; URLs to Images for testing
(define BRAD_PITT1 
  "http://upload.wikimedia.org/wikipedia/commons/thumb/5/51/Brad_Pitt_Fury_2014.jpg/220px-Brad_Pitt_Fury_2014.jpg")
(define BRAD_PITT2
  "http://upload.wikimedia.org/wikipedia/commons/thumb/c/c7/Brad_Pitt_Inglorious_Basterds_Berlin_premiere.jpg/170px-Brad_Pitt_Inglorious_Basterds_Berlin_premiere.jpg")
(define HARRY_POTTER1 
  "http://upload.wikimedia.org/wikipedia/commons/thumb/6/6e/Harry_Potter_wordmark.svg/250px-Harry_Potter_wordmark.svg.png")
(define HARRY_POTTER2
  "http://upload.wikimedia.org/wikipedia/commons/thumb/8/81/The_Elephant_House.jpg/200px-The_Elephant_House.jpg")

(define EMPTY-WORLD (make-world (list ) (list )))

(define BRAD-WORLD-POST 
  (make-world (list BRAD_PITT1) (list BRAD_PITT2)))

(define BRAD-WORLD-PRE 
  (make-world (list BRAD_PITT1) (list BRAD_PITT2)))

(define BRAD-WORLD-EMPTY 
  (make-world (list BRAD_PITT1 BRAD_PITT2) (list )))

(define HARRY-WORLD-POST 
  (make-world (list HARRY_POTTER1) (list HARRY_POTTER2)))

(define HARRY-WORLD-PRE 
  (make-world (list HARRY_POTTER1) (list HARRY_POTTER2)))

(define INITIAL-WORLD 
  (make-world (list) 
              (list BRAD_PITT1 BRAD_PITT2 HARRY_POTTER1 HARRY_POTTER2)))
;
; ====================== TEMPLATE =========================
#; (define (fn-xexpr xexpr)
     (cond
       [(symbol? xexpr) ...]
       [(string? xexpr) ...]
       [(number? xexpr) ...]
       [(cons? xexp) ... (fn-tag (first xexpr))...
                     ... (fn-sub-xexpr (rest xexpr))...]))

#; (define (fn-sub-xexpr subxexpr)
     (cond
       [(empty? subxexpr) ...]
       [(cons? subxexpr)(if (xexpr? (first subexpr) )
                            (fn-listofXexpr subexpr)
                            (fn-listofAttr (first subexpr)...)
                            (fn-listofXexpr (rest subexpr)...))]))

#; (define (fn-listofXexpr loxexpr)
     (cond
       [(empty? loxexpr)...]
       [(cons? loxexpr)...(fn-xexpr (first loxexpr)) ...
                       ...(fn-listofXexpr (rest loxexpr))...]))

#; (define (fn-attribute attri)
     ... (first attri) ...
     ... (rest attri) ...)

#; (define (fn-listofAttr loattri)
     (cond
       [(empty? loattri)...]
       [(cons? loattri) ...(fn-attribute (first loattri))...
                        ...(fn-listofAttr (rest loattri))...]))

#; (define (fn-world world)
     ... (world-pre world)...
     ... (world-post world) ...)

; ====================== FUNCTION IMPLEMENTATION =========================
; xexpr-element? : Any -> Boolean
; Returns true if xexpr is a valid XML element.
; STRATEGY: Structural decomposition
(define (xexpr-element? xexpr) 
  (or
   (symbol? xexpr)
   (string? xexpr)
   (number? xexpr)
   (and (cons? xexpr) 
        (symbol? (first xexpr))
        (is-subxexpr? (rest xexpr)))))

; TEST
(begin-for-test
  (check-equal? (xexpr-element? HTML-EMPTY) true)
  (check-equal? (xexpr-element? HTML-WITH-TEXT) true)
  (check-equal? (xexpr-element? (cons 2 '())) false)
  (check-equal? (xexpr-element? '()) false))

; is-subxexpr?: Any -> Boolean
; RETURN true if the subxexpr is a SubXexpr
; STRATEGY: Structural decomposition
(define (is-subxexpr? subxexpr)
  (cond
    [(empty? subxexpr) true]
    [(cons? subxexpr)(is-subxexpr-not-empty subxexpr)]))

; TEST
(begin-for-test 
  (check-equal? (is-subxexpr? (list XEXPR1 XEXPR2)) true)
  (check-equal? (is-subxexpr? '()) true)
  (check-equal? (is-subxexpr? (list XEXPR1)) true))

; is-subxexpr-not-empty: Any -> Boolean
; RETURN true if the given non-empty list is a SubXexpr
; STRATEGY: Strucutural decomposition
(define (is-subxexpr-not-empty subxexpr)
  (if (xexpr-element? (first subxexpr))
      (is-list-of-xexpr? subxexpr)
      (and (is-list-of-attribute? (first subxexpr))
           (is-list-of-xexpr? (rest subxexpr)))))

; is-list-of-xexpr?: Any -> Boolean
; RETURN true if the given variable is a ListOf<Xexpr>
; STRATEGY: Structural decomposition
(define (is-list-of-xexpr? loxexpr)
  (andmap xexpr-element? loxexpr))

; TEST
(begin-for-test
  (check-equal? (is-list-of-xexpr? LIST-OF-XEXPR) true))

; is-list-of-attribute? Any -> Boolean
; RETURN true if the given variable is a ListOf<Attribute>
; STRATEGY: Structural decomposition
(define (is-list-of-attribute? loattr)
  (and (cons? loattr)
       (andmap is-attribute? loattr)))

; is-attribute? : Any -> Boolean
; RETURN true if the given variable is a Attribute
; STRATEGY: Structural decomposition
(define (is-attribute? attr)
  (and
   (cons? attr)
   (symbol? (first attr))
   (string? (second attr))
   (= 2 (length attr))))

; xexpr-tag : Xexpr -> Maybe<Tag>
; Returns the tag of element xe.
; GIVEN an Xexpr
; STRATEGY: Structural decomposition
(define (xexpr-tag xe) 
  (if (cons? xe) 
      (first xe)
      false))

; TEST
(begin-for-test
  (check-equal? (xexpr-tag HTML-EMPTY) 'html)
  (check-equal? (xexpr-tag XEXPR1) false))

; xexpr-attributes : Xexpr -> ListOf<Attribute>
; Returns the attributes of the given XML element.
; GIVEN an Xexpr 
; WHERE Xexpr must have a listOf<Attribute>
; STRATEGY: Structural decomposition.
(define (xexpr-attributes xexpr)
  (if (and (cons? xexpr)
           (is-list-of-attribute? (second xexpr)))
      (second xexpr)
      '()))

; TEST
(begin-for-test
  (check-equal? (xexpr-attributes HTML-EMPTY)
                '((lang "en-US")))
  (check-equal? (xexpr-attributes HTML-WITH-TEXT)
                '((lang "en-US"))))

; xexpr-all-attributes: Xexpr -> ListOf<Attribute>
; Returns all the attributes of the given XML element.
; GIVEN an Xexpr 
; WHERE Xexpr must have a listOf<Attribute>
; STRATEGY: Structural decomposition.
(define (xexpr-all-attributes xexpr) 
  (if (cons? xexpr)
      (get-attribute-list (rest xexpr))
      '()))

; TEST
(begin-for-test
  (check-equal? (xexpr-all-attributes HTML-EMPTY)
                '((lang "en-US")))
  (check-equal? (xexpr-all-attributes HTML-WITH-TEXT)
                '((lang "en-US"))))

; get-attribute-list: SubXexpr -> listOf<Attribute>
; RETURN a list contains all the attributes in the SubXexpr.
; STRATEGY: Structural decomposition
(define (get-attribute-list subxexpr)
  (cond
    [(empty? subxexpr) '()]
    [(cons? subxexpr) (get-attribute-list-non-empty subxexpr)]))

; get-attribute-list-non-empty: SubXexpr -> listOf<Attribute>
; GIVEN: a non-empty SubXexpr.
; RETURNS: a list contains all the attributes in the SubXexpr.
; STRATEGY: Structural decomposition
(define (get-attribute-list-non-empty subxexpr)
  (if (is-list-of-attribute? (first subxexpr)) 
      (append (first subxexpr)
              (get-attribute-list-from-list-of-xexpr (rest subxexpr)))
      (get-attribute-list-from-list-of-xexpr subxexpr)))

; TEST
(begin-for-test
  (check-equal? (get-attribute-list SUBXEXPR-HTML-EMPTY)
                '((lang "en-US")))
  (check-equal? (get-attribute-list SUBXEXPR-HTML-WITH-TEXT)
                '((lang "en-US")))
  (check-equal? (get-attribute-list (list XEXPR3))
                '((height "10")
                  (width "20")
                  (center "100"))))


; xexpr-content : Xexpr -> ListOf<Xexpr>
; Extracts the content of an XML element.
; An element's tag and attributes are not part of its content.
; WHERE
;  - The Xexpr must contains a list of Xexpression
; STRATEGY: Structural decomposition
(define (xexpr-content xe)
  (if (cons? xe) 
      (get-xexpr-list (rest xe))
      '()))

; TEST
(begin-for-test
  (check-equal? (xexpr-content HTML-EMPTY)
                '((body (p) (br) (p) (br))))
  (check-equal? (xexpr-content HTML-WITH-TEXT)
                '((body
                   (p "Here is assignment " 5)
                   (br)
                   (p "This is a " (b "data") " definition.")))))


; get-xexpr-list: SubXexpr -> ListOf<Xexpr>
; RETURN A list of all the Xexpressions in the SubXexpression
; STRATEGY: Structural decomposition
(define (get-xexpr-list subxexpr)
  (cond
    [(empty? subxexpr) '()]
    [(is-list-of-attribute? (first subxexpr)) (rest subxexpr)]
    [else subxexpr]))

; TEST
(begin-for-test
  (check-equal?
   (get-xexpr-list LIST-OF-XEXPR)
   (list XEXPR1 XEXPR2 XEXPR3)))

; get-attribute-list-from-list-of-xexpr: ListOf<Xexpr> -> ListOf<Attribute>
; RETURN a list of all the Attributes in the list
; STRATEGY: Structural decomposition
(define (get-attribute-list-from-list-of-xexpr loxexpr)
  (cond
    [(empty? loxexpr) '()]
    [(cons? loxexpr) 
     (append (xexpr-all-attributes (first loxexpr))
             (get-attribute-list-from-list-of-xexpr (rest loxexpr)))]))

; attribute-value : ListOf<Attribute> AttrName -> Maybe<AttrValue>
; Returns the value of *first* attribute in loa named aname.
; Returns #f if no attribute in loa has the name aname.
; STRATEGY: Structural decomposition
(define (attribute-value loattr aname)
  (if (false? (get-matching-attribute loattr aname))
      false
      (second (get-matching-attribute loattr aname))))

; TEST
(begin-for-test
  (check-equal? (attribute-value LIST-OF-ATTRI 'lan)
                false)
  (check-equal? (attribute-value LIST-OF-ATTRI 'lang)
                "en-US"))

; get-matching-attribute: ListOf<Attribute> AttriName -> Maybe<Attribute>
; RETURN #f if there is no matching Attribute
; RETURN the *first* Attribute if there is an attribute which matches
; the given Attribute name
; STRATEGY: Structural decomposition
(define (get-matching-attribute loa aname)
  (if (empty? (get-all-matching-attributes loa aname))
      false
      (first (get-all-matching-attributes loa aname))))

; get-all-matching-attributes: ListOf<Attribute> AttriName -> ListOf<Attribute>
; RETURN empty if there is no matching Attribute
; RETURN all the Attributes if their name is the same as the AttriName
; STRATEGY: Structural decomposition
(define (get-all-matching-attributes loa aname)
  (filter (lambda (attr) (same-attr-name? attr aname)) loa))

; same-attr-name?: AttriName AttriName -> Boolean
; RETURN true if the two names are the same, false otherwise
; STRATEGY: Function composition
(define (same-attr-name? attr aname)
  (symbol=? (first attr) aname))

; get-value : Xexpr AttrName -> Maybe<AttrValue>
; Returns the value of the first attribute in xe named aname.
; STRATEGY: Structural Decomposition
(define (get-value xe aname)
  (if (and (cons? xe)(is-subxexpr? (rest xe)))
      (get-value-from-attr (get-attribute-list (rest xe)) aname)
      false))

; TEST
(begin-for-test
  (check-equal? (get-value HTML-EMPTY 'lang) "en-US")
  (check-equal? (get-value HTML-EMPTY 'lan) false)
  (check-equal? (get-value 3 'lang) false))

; get-value-from-attr: Maybe<ListOf<Attribute>> AttrName -> Maybe<AttrValue>
; RETURN false if the given Attribute list is empty or false
; STRATEGY: Structural Decomposition
(define (get-value-from-attr maybeloattr aname)
  (if (empty? maybeloattr)
      false
      (attribute-value maybeloattr aname)))

; TEST
(begin-for-test
  (check-equal? (get-value-from-attr '() 'lang) false)
  (check-equal? (get-value-from-attr LIST-OF-ATTRI 'lang) "en-US"))

; xexpr-find : Xexpr AttrName -> ListOf<AttrValue>
; Searches xe and nested elements in xe for attributes named aname
; and returns all the values for these attributes.
; STRATEGY: Structural Decomposition
(define (xexpr-find xexpr aname)
  (if (cons? xexpr)
      (get-attrval-list-from-subxexpr (rest xexpr) aname)
      '()))

; TEST
(begin-for-test 
  (check-equal? (xexpr-find HTML-EMPTY 'lang)
                '("en-US"))
  (check-equal? (xexpr-find XEXPR3 'height)
                '("10")))

; get-attrval-list-from-subxexpr: SubXexpr AttrName -> ListOf<AttrValue>
; RETURN a list of Attribute values given a subXexpression
; STRATEGY: Function Composition
(define (get-attrval-list-from-subxexpr subxexpr aname)
  (map second (get-matching-attr-list-from-subxexpr subxexpr aname)))

; TEST
(begin-for-test
  (check-equal?
   (get-attrval-list-from-subxexpr SUBXEXPR-HTML-EMPTY 'lang)
   '("en-US"))
  (check-equal?
   (get-attrval-list-from-subxexpr (list XEXPR3) 'height)
   '("10")))

; get-matching-attr-list-from-subxexpr:  SubXexpr AttrName-> ListOf<Attr>
; RETURN a list of Attribute with the same Name as the given AttrName
; given a SubXexpr
; STRATEGY: Function Composition
(define (get-matching-attr-list-from-subxexpr subxexpr aname)
  (get-all-matching-attributes (get-attr-list-from-subxexpr subxexpr) aname))

; get-attr-list-from-subxexpr: SubXexpr -> ListOf<Attr>
; RETURN the whole list of the Attributes in the SubXexpr,
; RETURN empty if there is no element in the list
; STRATEGY: Function Composition
(define (get-attr-list-from-subxexpr subxexpr)
  (if
   (false? (get-attribute-list subxexpr))
   '()
   (get-attribute-list subxexpr)))

; xexpr-elements : Xexpr Tag -> ListOf<Xexpr>
; Returns a list of all elements in xe whose tag is t.
; An element with tag t may be nested inside another element with 
; the same tag and this function returns both.
; STRATEGY: Function composition.
(define (xexpr-elements xexpr tag)
  ; Returns false if an element is empty, else returns true.
  ; Used to filter out all empty lists in the list.
  (filter (lambda (x) (not (empty? x))) 
          (get-all-xexpr-elements xexpr tag))) 

; get-all-xexpr-elements: Xexpr Tag -> ListOf<Xexpr>
; RETURN a list of all elements in xe whose tag is t.
; An element with tag t may be nested inside another element with 
; the same tag and this function returns both.
; STRATEGY: Structural decomposition
(define (get-all-xexpr-elements xexpr tag)
  (cond
    [(symbol? xexpr) '()]
    [(string? xexpr) '()]
    [(number? xexpr) '()]
    [(empty? xexpr) '()]
    [(cons? xexpr) (cons (xexpr-with-matching-tag xexpr tag)
                         (search-tag-in-xexpr-list 
                          (get-xexpr-list (rest xexpr)) tag))]))

; search-tag-in-xexpr-list: ListOf<Xexpr> Tag -> ListOf<Xexpr>
; Finds Xexpr's in the given ListOf<Xexprs> whose Tag matches 
; the given Tag.
; STRATEGY: Structural decomposition.
(define (search-tag-in-xexpr-list list-of-xexpr tag)
  (cond
    [(empty? list-of-xexpr) '()]
    [(cons? list-of-xexpr) 
     (append (get-all-xexpr-elements (first list-of-xexpr) tag)
             (search-tag-in-xexpr-list (rest list-of-xexpr) tag))]))

; TEST
(begin-for-test
  (check-equal? (search-tag-in-xexpr-list LIST-OF-XEXPR 'level3) 
                (list empty XEXPR3 empty))
  (check-equal? (search-tag-in-xexpr-list LIST-OF-XEXPR 'level2) 
                (list XEXPR2 empty XEXPR2)))

; xexpr-with-matching-tag : Xexpr Tag -> XexprOrEmpty
; RETURNS the same Xexpr if the tag of the Xexpr matches the given tag.
; Else, returns an empty list.
; STRATEGY: Function composition.
(define (xexpr-with-matching-tag xexpr tag)
  (if (symbol=? (xexpr-tag xexpr) tag)
      xexpr
      '()))

; TEST
(begin-for-test
  (check-equal?
   (xexpr-elements XEXPR3 'level2)
   (list XEXPR2))
  (check-equal?
   (xexpr-elements XEXPR2 'level2)
   (list XEXPR2))
  (check-equal?
   (xexpr-elements XEXPR3 'level3)
   (list XEXPR3)))


; ======================= xexpr-depth ========================

; xexpr-depth : Xexpr -> Natural
; Returns the depth of the deepest nested Xexpr. 
; An Xexpr with no nested elements has depth 0.
; STRATEGY: Structural Decomposition
(define (xexpr-depth xexpr)
  (cond
    [(symbol? xexpr) 0]
    [(string? xexpr) 0]
    [(number? xexpr) 0]
    [(cons? xexpr) (subxexpr-depth (rest xexpr))]))

; TEST
(begin-for-test
  (check-equal? (listofXexpr-depth (list XEXPR1)) 0)
  (check-equal? (listofXexpr-depth (list XEXPR2)) 1)
  (check-equal? (listofXexpr-depth (list XEXPR3)) 2)
  (check-equal? (subxexpr-depth SUBXEXPR-HTML-EMPTY) 2)
  (check-equal? (subxexpr-depth SUBXEXPR-HTML-WITH-TEXT) 4)
  (check-equal? (xexpr-depth HTML-EMPTY) 2)
  (check-equal? (xexpr-depth HTML-WITH-TEXT) 4)
  (check-equal? (xexpr-depth XEXPR3) 2)
  (check-equal? (xexpr-depth XEXPR2) 1)
  (check-equal? (xexpr-depth XEXPR1) 0)
  (check-equal? (xexpr-depth HTML-ONLY-ATTR) 0))

; subxexpr-depth: Sub-xexpr -> Natural
; RETURN the depth of the deepest nested Xexpr
; in the given Sub-xexpr.
; STRATEGY: Structural Decomposition
(define (subxexpr-depth subxexpr)
  (cond
    [(empty? subxexpr) 0]
    [(cons? subxexpr) (subxexpr-depth-check-xexpr subxexpr)]))

; listofXexpr-depth: ListOf<Xexpr> -> Natural
; RETURN the deepest nested Xexpr among
; the given ListOf<Xexpr>
; STRATEGY: Structural Decomposition
(define (listofXexpr-depth loxexpr)
  (cond
    [(empty? loxexpr) 0]
    [(cons? loxexpr) (listofXexpr-deepest-depth loxexpr)]))

; listofXexpr-deepest-depth: ListOf<Xexpr> -> Natural
; RETURN the deepest nested Xexpr among
; the given Non-empty ListOf<Xexpr>
; STRATEGY: Structural Decomposition
(define (listofXexpr-deepest-depth loxexpr)
  (xexpr-depth 
   (first 
    (sort loxexpr (lambda (x y) (> (xexpr-depth x) (xexpr-depth y)))))))

; subxexpr-depth-check-xexpr: Sub-xexpr -> Natural
; RETURN the depth of the deepest nested Xexpr
; in the given non-empty Sub-xexpr.
; STRATEGY: Structural Decomposition
(define (subxexpr-depth-check-xexpr subxexpr)
  (if (xexpr-element? (first subxexpr))
      (+ 1 (listofXexpr-depth subxexpr))
      (subxexpr-second-depth subxexpr)))

; subxexpr-second-depth: Sub-xexpr -> Natural
; RETURN the depth of the deepest nested xexpr 
; GIVEN a Sub-xexpr which contains ListOf<Attribute>.
; STRATEGY: Structural Decomposition
(define (subxexpr-second-depth subxexpr)
  (cond
    [(empty? (rest subxexpr)) 0]
    [else (+ 1 (listofXexpr-depth (rest subxexpr)))]))


; ========================== BIG BANG ===========================

(define (wiki-pictures firstname lastname)
  (local ((define URL (string-append PREFIX firstname "_" lastname))
          
          (define (get-initial-world url)
            (make-world 
             '()
             (get-initial-url-list URL))))
    ; - MAIN -
    (big-bang (get-initial-world URL)
              [on-tick next-world 5]
              [to-draw render]
              [name firstname])))


; ========================== FETCHING IMAGES =========================== 
; get-initial-url-list : URL -> ListOf<URL>
; GIVEN: A URL for a page.
; RETURNS: A list of the URLs to all the images in the page.
; STRATEGY: Function composition.
(define (get-initial-url-list url)
  (filter-small-images 
   (map (lambda (x) (string-append "http:" x))
        (get-image-urls url))))

; filter-small-images : ListOf<URL> -> ListOf<URL>
; Filters out the URL of small images from the given URL List 
; and Returns the large image URLs.
; STRATEGY: Function composition.
(define (filter-small-images url-list)
  (local (
          ; large-image? : URL -> Boolean
          ; RETURNS #true if the image height is larger than the MIN-IMG-HEIGHT
          (define (large-image? url) 
            (< MIN-IMG-HEIGHT (image-height (bitmap/url url)))))
    (filter large-image? url-list)))

; get-image-urls URL -> ListOf<URL>
; GIVEN: A URL for a page.
; RETURNS: A list of the URLs to all the images in the page.
; if the URL exists.
; STRATEGY: Function composition.
(define (get-image-urls url)
  (if (false? (url-exists? url))
  '()
  (xexpr-find 
   (xexpr-elements 
    (read-xexpr/web url) 
    'img) 
   'src)))

(begin-for-test
  (check-equal? 
   (first (get-initial-url-list "http://en.wikipedia.org/wiki/Barack_Obama"))
   "http://upload.wikimedia.org/wikipedia/commons/thumb/8/8d/President_Barack_Obama.jpg/220px-President_Barack_Obama.jpg")
  "Must fetch the correct large images")

; ==================== RENDER =====================

; render-image: World -> Image
; RETURN an image based on one World which
; is the first element on the Given list
; structural decomposition
(define (render-image world)
  (if (and (empty? (world-pre world))
           (empty? (world-post world)))
      (text NO-IMAGE-INFO FONT-SIZE FONT-COLOR)
      (render-image-pics world)))

; render: World -> Image
; functional composition
(define (render world)
  (place-image (render-image world)
               CENTER-X
               CENTER-Y
               BACKGROUND))

; render-image-pics: World -> Image
; RETURN an Image based on the given World
; if it's order is true then draw the pre list
; of URL, otherwise draw post list of URL
; structural decomposition
(define (render-image-pics world)
  (render-from-list (world-post world)))

; render-from-list: ListOf<URL> -> Image
; RETURN an Image from the first of the given list of URL
; structural decomposition
(define (render-from-list url-list)
  (cond
    [(empty? url-list) ONE-MORE-ROUND]
    [(cons? url-list) (bitmap/url (first url-list))]))


(begin-for-test
  (check-equal? 
   (render EMPTY-WORLD) 
   (place-image 
    (text NO-IMAGE-INFO FONT-SIZE FONT-COLOR) 
    CENTER-X CENTER-Y BACKGROUND)
   "Render empty image")
  (check-equal? 
   (render BRAD-WORLD-POST) 
   (place-image (bitmap/url BRAD_PITT2) CENTER-X CENTER-Y BACKGROUND)
   "Rendering from Post")
  (check-equal? 
   (render BRAD-WORLD-EMPTY) 
   (place-image ONE-MORE-ROUND CENTER-X CENTER-Y BACKGROUND)
   "Rendering One More Round"))

; ==================== next-world ====================

; next-world: World -> World
; RETURN a new World 
; WHERE 
; - the pre-list put one to post-list
; or post-list put one to pre-list part 
; depending on the current order
; - if the pre is empty or the post is empty;
; switch the order field
; - if both are empty, the world stays with no change
; structural decomposition
(define (next-world world)
  (if (and (empty? (world-pre world))
           (empty? (world-post world)))
      world
      (loop-through-images world)))

(begin-for-test
  (check-equal? (next-world (make-world (list ) (list 4 5 6 1 2 3)))
                (make-world (list 4) (list 5 6 1 2 3)))
  (check-equal? (next-world (make-world (list 4 5 6 1 2 3) (list )))
                (make-world (list ) (list 4 5 6 1 2 3)))
  (check-equal? (next-world (make-world (list ) (list )))
                (make-world (list ) (list))))


; loop-through-images: World -> World
; Remove one element from one list and add to another list
; STRATEGY: Structural decomposition on world : World
(define (loop-through-images world)
  (if (empty? (world-post world))
      (switch-pre-post world)
      (from-post-to-pre world)))

; from-post-to-pre: World -> World
; GIVEN A World where the world is moving the element from the post
; list to the pre list;
; RETURN a new World where the post-list's first element removed and 
; and added to the pre list
; STRATEGY: structural decomposition on world : World
(define (from-post-to-pre world)
  (if (empty? (world-post world))
      (switch-pre-post world)
      (transfer-from-post-to-pre (world-pre world) (world-post world))))

; transfer-from-post-to-pre: ListOf<URL> ListOf<URL> -> World
; RETURN a new World where the element is transfered from the 
; world's post-list to pre-list
; STRATEGY: Structural decomposition on post-list : ListOf<URL>
(define (transfer-from-post-to-pre pre-list post-list)
  (make-world (remove-from-add-to post-list pre-list)
              (rest post-list)))

; swap-world-order: World -> World
; RETURN a world where the order of transfer changed
; STRATEGY: Structural decomposition on world : World.
(define (switch-pre-post world)
  (make-world (world-post world)
              (world-pre world)))

; remove-from-add-to: ListOf<URL> ListOf<URL> -> ListOf<URL>
; RETURN the to-list where the the list element from the from-list
; tranfers to the to-list
; STRATEGY: Structural decomposition on from-list  : ListOf<URL>
(define (remove-from-add-to from-list to-list)
  (reverse (cons (first from-list) (reverse to-list))))



; ======================Alternate Data definitions==================
;  The new Data definition using XWords is:

;  An Xexpr is one of:
;   - Symbol
;   - XWord
;   - Number
;   - (cons Tag (cons ListOf(Atrribute) ListOf<Xexpr>))
;   - (cons Tag ListOf<Xexpr>)

; A XWord is '(word ((text String))).

; The first function which is affected is:
;  1) xexpr-element? : 
;   An additional function to check from XWord would be required instead of using string?

;   XWord? would check for the following:
;    - The first element is a symbol?
;    - The second is a list of a ('text "String") pair.

; 2) xexpr-tag :
;   An additional check is (is-subxexpr? (rest xexpr)) would be required
;   as the XWord is also a cons?.
;   Only if the rest is a SubXexpr, the first value is a Tag.