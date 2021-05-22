;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex376) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An Xexpr is a list: 
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; – (cons Symbol (cons XWord Body))
; – (cons Symbol (cons [List-of Attribute] (cons Xword Body)))
; where Body is short for [List-of Xexpr].

; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; An XorA is one of:
; – Xexpr
; – [List-of Attribute]

; An XWord is '(word ((text String))).

; An XItem.v2 is one of: 
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons [List-of Attribute] (list XWord)))
; – (cons 'li (cons XEnum.v2 '()))
; – (cons 'li (cons [List-of Attribute] (list XEnum.v2)))
; 
; An XEnum.v2 is one of:
; – (cons 'ul [List-of XItem.v2])
; – (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define WORD1 '(word ((text "word1"))))
(define WORD2 '(word ((text "word2"))))
(define WORD3 '(word ((text "word3"))))
(define WORD4 '(word ((text "hello"))))
(define WORD5 '(word ((text "Hello"))))
(define WORD6 '(word ((text "helo"))))
(define WORD7 '(word ((text "hello"))))

(define ATTR1 (list 'attribute1 "attribute1"))
(define ATTR2 (list 'attribute2 "attribute2"))
(define ATTR3 (list 'attribute3 "attribute3"))

(define LOA (list ATTR1 ATTR2 ATTR3))

(define XITEM1 (list 'li WORD1))
(define XITEM2 (list 'li LOA WORD1))
(define XITEM3 (list 'li WORD2))
(define XITEM4 (list 'li WORD3))
(define XITEM5 (list 'li LOA WORD4))
(define XITEM6 (list 'li WORD5))
(define XITEM7 (list 'li LOA WORD6))
(define XITEM8 (list 'li LOA WORD7))

(define XENUM1 (list 'ul
                     XITEM1
                     XITEM2
                     (list 'li LOA WORD2)
                     (list 'li WORD3)))
(define XENUM2 (list 'ul
                     LOA
                     XITEM1
                     XITEM2
                     XITEM3
                     XITEM4
                     XITEM5
                     XITEM6
                     XITEM7
                     XITEM8))

(define XITEM9 (list 'li XENUM1))
(define XITEM10 (list 'li LOA XENUM2))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; String XEnum.v2 -> Number
; Count all occurrences of str in enum.
(define (count-enum str enum)
  (local ((define CONTENT (xexpr-content enum)))
    ; – IN –
    (foldl (lambda (x y) (+ (count-item str x) y)) 0 CONTENT)))

(check-expect (count-enum "zero" XENUM2)
              0)
(check-expect (count-enum "hello" XENUM1)
              0)
(check-expect (count-enum "hello" XENUM2)
              2)

; String XItem.v2 -> Number
; If the String in itm is equal to str,
; return 1, 0 otherwise.
(define (count-item str itm)
  (local ((define CONTENT (first (xexpr-content itm))))
    ; – IN –
    (cond
      [(word? CONTENT)
       (if (string=? (word-text CONTENT) str)
           1
           0)]
      [else
       (count-enum str CONTENT)])))
    

(check-expect (count-item "hello" XITEM5)
              1)
(check-expect (count-item "hello" XITEM6)
              0)
(check-expect (count-item "hello" XITEM7)
              0)
(check-expect (count-item "hello" XITEM9)
              0)
(check-expect (count-item "hello" XITEM10)
              2)

; from ex370.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Any -> Boolean
; Is w an XWord?
(define (word? w)
  (and
   ; determine limits of w, i.e. w has length of 2
   (cons? w)
   (cons? (rest w))
   (empty? (rest (rest w)))
   ; first element of w
   (symbol? (first w))
   (symbol=? (first w) 'word)
   ; second element of w
   ; limits
   (cons? (first (rest w)))    ; (list (list 'text String))
   (cons? (first (first (rest w))))    ; (list 'text String)
   (empty? (rest (first (rest w))))
   (empty? (rest (rest (first (first (rest w))))))
   ; content of the second element of w
   (symbol? (first (first (first (rest w)))))
   (symbol=? (first (first (first (rest w)))) 'text)
   (string? (first (rest (first (first (rest w))))))))

; XWord -> String
; Extract the value of xw.
(define (word-text xw)
  (second (first (second xw))))

; from ex368.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; XorA -> Boolean
; Is x a list of attributes?
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

; from ex366.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Xexpr -> [List-of Xexpr]
; Retrieve the list of content elements of xe.
(define (xexpr-content xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-content (first optional-loa+content)))
         (if (list-of-attributes? loa-or-content)
             (rest optional-loa+content)
             optional-loa+content))])))