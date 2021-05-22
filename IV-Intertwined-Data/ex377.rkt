;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex377) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; An XWorXE is one of:
; – XWord
; – XEnum.v2

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define WORD1 '(word ((text "word1"))))
(define WORD2 '(word ((text "word2"))))
(define WORD3 '(word ((text "word3"))))
(define WORD4 '(word ((text "hello"))))
(define WORD5 '(word ((text "Hello"))))
(define WORD6 '(word ((text "helo"))))
(define WORD7 '(word ((text "hello"))))
(define WORD8 '(word ((text "hello"))))

(define ATTR1 (list 'attribute1 "attribute1"))
(define ATTR2 (list 'attribute2 "attribute2"))
(define ATTR3 (list 'attribute3 "attribute3"))

(define LOA (list ATTR1 ATTR2 ATTR3))

(define XITEM1 (list 'li WORD1))
(define XITEM2 (list 'li LOA WORD2))
(define XITEM3 (list 'li WORD3))
(define XITEM4 (list 'li WORD4))
(define XITEM5 (list 'li LOA WORD5))
(define XITEM6 (list 'li WORD6))
(define XITEM7 (list 'li LOA WORD7))
(define XITEM8 (list 'li LOA WORD8))

(define XITEM44 (list 'li '(word ((text "bye")))))
(define XITEM77 (list 'li LOA '(word ((text "bye")))))
(define XITEM88 (list 'li LOA '(word ((text "bye")))))

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
(define XENUM3 (list 'ul
                     LOA
                     XITEM1
                     XITEM2
                     XITEM3
                     XITEM44
                     XITEM5
                     XITEM6
                     XITEM77
                     XITEM88))

(define XITEM9 (list 'li XENUM1))
(define XITEM10 (list 'li LOA XENUM2))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; String String XEnum.v2 -> XEnum.v2
; If any of the items in enum cotain the value str1,
; replace them with str2.
(define (replace-enum str1 str2 enum)
  (local ((define LOA-or-CONTENT (rest enum)))
    ; – IN –
    (if (list-of-attributes? (first LOA-or-CONTENT))
        (cons (first enum)
              (cons (first LOA-or-CONTENT)
              (map (lambda (x) (replace-item str1 str2 x))
                   (rest LOA-or-CONTENT))))
        (cons (first enum)
              (map (lambda (x) (replace-item str1 str2 x))
                   LOA-or-CONTENT)))))

(check-expect (replace-enum "hello" "bye" XENUM1)
              XENUM1)
(check-expect (replace-enum "hello" "bye" XENUM2)
              XENUM3)

; String String XItem.v2 -> XItem.v2
; If the String(s) in itm is (are) equal to str1,
; replace it with str2.
(define (replace-item str1 str2 itm)
  (local ((define LOA-or-CONTENT (rest itm))

          ; XWorXE -> XWorXE
          ; Replace all occurrences of str1 in xworxe with str2.
          (define (replace xworxe)
            (cond
              [(word? xworxe)
               (if (string=? (word-text xworxe) str1)
                   `(word ((text ,str2)))
                   xworxe)]
              [else
               (replace-enum str1 str2 xworxe)])))
    ; – IN –
    (if (list-of-attributes? (first LOA-or-CONTENT))
        (list (first itm)
              (first LOA-or-CONTENT)
              (replace (first (rest LOA-or-CONTENT))))
        (list (first itm)
              (replace (first LOA-or-CONTENT))))))

(check-expect (replace-item "hello" "bye" XITEM3)
              XITEM3)
(check-expect (replace-item "hello" "bye" XITEM4)
              (list 'li '(word ((text "bye")))))
(check-expect (replace-item "hello" "bye" XITEM5)
              XITEM5)
(check-expect (replace-item "hello" "bye" XITEM6)
              XITEM6)
(check-expect (replace-item "hello" "bye" XITEM9)
              XITEM9)
(check-expect (replace-item "hello" "bye" XITEM10)
              (list 'li LOA XENUM3))

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