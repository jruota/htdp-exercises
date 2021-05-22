;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex373) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define SIZE 24) ; font size 
(define COLOR "orange") ; font color 
(define BT ; a graphical constant 
  (beside (circle 2 'solid COLOR) (text " " SIZE COLOR)))

(define WORD1 '(word ((text "word1"))))
(define WORD2 '(word ((text "word2"))))
(define WORD3 '(word ((text "word3"))))

(define ATTR1 (list 'attribute1 "attribute1"))
(define ATTR2 (list 'attribute2 "attribute2"))
(define ATTR3 (list 'attribute3 "attribute3"))

(define LOA (list ATTR1 ATTR2 ATTR3))

(define XITEM1 (list 'li WORD1))
(define XITEM2 (list 'li LOA WORD1))

(define XENUM1 (list 'ul
                     XITEM1
                     XITEM2
                     (list 'li LOA WORD2)
                     (list 'li WORD3)))
(define XENUM2 (list 'ul
                     LOA
                     XITEM1
                     XITEM2))

(define XITEM3 (list 'li XENUM1))
(define XITEM4 (list 'li LOA XENUM2))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Image -> Image
; marks item with bullet  
(define (bulletize item)
  (beside/align 'center BT item))

(check-expect (bulletize (rectangle 60 20 "solid" "orange"))
              (beside/align "center" BT (rectangle 60 20 "solid" "orange")))
 
; XEnum.v2 -> Image
; renders an XEnum.v2 as an image 
(define (render-enum xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v2 Image -> Image 
          (define (deal-with-one item so-far)
            (above/align 'left (render-item item) so-far)))
    (foldr deal-with-one empty-image content)))

(check-expect (render-enum XENUM1)
              (above
               (beside/align "center"
                             (beside (circle 2 'solid COLOR)
                                     (text " " SIZE COLOR))
                             (text "word1" SIZE COLOR))
               (beside/align "center"
                             (beside (circle 2 'solid COLOR)
                                     (text " " SIZE COLOR))
                             (text "word1" SIZE COLOR))
               (beside/align "center"
                             (beside (circle 2 'solid COLOR)
                                     (text " " SIZE COLOR))
                             (text "word2" SIZE COLOR))
               (beside/align "center"
                             (beside (circle 2 'solid COLOR)
                                     (text " " SIZE COLOR))
                             (text "word3" SIZE COLOR))))
(check-expect (render-enum XENUM2)
              (above
               (beside/align "center"
                             (beside (circle 2 'solid COLOR)
                                     (text " " SIZE COLOR))
                             (text "word1" SIZE COLOR))
               (beside/align "center"
                             (beside (circle 2 'solid COLOR)
                                     (text " " SIZE COLOR))
                             (text "word1" SIZE COLOR))))
 
; XItem.v2 -> Image
; renders one XItem.v2 as an image 
(define (render-item an-item)
  (local ((define content (first (xexpr-content an-item))))
    (bulletize
      (cond
        [(word? content)
         (text (word-text content) SIZE COLOR)]
        [else (render-enum content)]))))

(check-expect (render-item XITEM1)
              (beside/align "center"
                            (beside (circle 2 'solid COLOR)
                                    (text " " SIZE COLOR))
                            (text "word1" SIZE COLOR)))
(check-expect (render-item XITEM2)
              (beside/align "center"
                            (beside (circle 2 'solid COLOR)
                                    (text " " SIZE COLOR))
                            (text "word1" SIZE COLOR)))
(check-expect (render-item XITEM3)
              (beside/align "center"
                            (beside (circle 2 'solid COLOR)
                                    (text " " SIZE COLOR))
                            (above
                             (beside/align "center"
                                           (beside (circle 2 'solid COLOR)
                                                   (text " " SIZE COLOR))
                                           (text "word1" SIZE COLOR))
                             (beside/align "center"
                                           (beside (circle 2 'solid COLOR)
                                                   (text " " SIZE COLOR))
                                           (text "word1" SIZE COLOR))
                             (beside/align "center"
                                           (beside (circle 2 'solid COLOR)
                                                   (text " " SIZE COLOR))
                                           (text "word2" SIZE COLOR))
                             (beside/align "center"
                                           (beside (circle 2 'solid COLOR)
                                                   (text " " SIZE COLOR))
                                           (text "word3" SIZE COLOR)))))
(check-expect (render-item XITEM4)
              (beside/align "center"
                            (beside (circle 2 'solid COLOR)
                                    (text " " SIZE COLOR))
                            (above
                             (beside/align "center"
                                           (beside (circle 2 'solid COLOR)
                                                   (text " " SIZE COLOR))
                                           (text "word1" SIZE COLOR))
                             (beside/align "center"
                                           (beside (circle 2 'solid COLOR)
                                                   (text " " SIZE COLOR))
                                           (text "word1" SIZE COLOR)))))

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