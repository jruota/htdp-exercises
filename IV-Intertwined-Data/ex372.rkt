;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex372) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; An XEnum.v1 is one of: 
; – (cons 'ul [List-of XItem.v1])
; – (cons 'ul (cons Attributes [List-of XItem.v1]))

; An XItem.v1 is one of:
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons Attributes (cons XWord '())))

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define BT (beside (circle 3 "solid" "black")
                   (rectangle 5 0 "outline" "transparent")))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; XItem.v1 -> Image 
; Render an item as a "word" prefixed by a bullet.
(define (render-item1 i)
  (local ((define content (xexpr-content i))
          (define element (first content))
          (define a-word (word-text element))
          (define item (text a-word 12 'black)))
    (beside/align 'center BT item)))

(check-expect (render-item1 (cons 'li (cons '(word ((text "word"))) '())))
              (beside/align "center"
                            (beside (circle 3 "solid" "black")
                                    (rectangle 5 0 "solid" "transparent"))
                            (text "word" 12 "black")))

; Explain how the function works.

; 1) Treat the XItem like an Xexpr and get its content,
;    i.e. get rid of 'li and possible attributes.
; 2) Select the XWord from the content.
; 3) Extract the string from XWord.
; 4) Turn the string into an image.
; 5) Align the text image with the bullet.

; from ex370.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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