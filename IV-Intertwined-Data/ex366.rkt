;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex366) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An Xexpr is a list: 
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr]

; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define a0 '((initial "X")))
 
(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Xexpr -> Symbol
; Retrieve the name of xe.
(define (xexpr-name xe)
  (first xe))

(check-expect (xexpr-name e0)
              'machine)
(check-expect (xexpr-name e1)
              'machine)
(check-expect (xexpr-name e2)
              'machine)
(check-expect (xexpr-name e3)
              'machine)
(check-expect (xexpr-name e4)
              'machine)

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

(check-expect (xexpr-content e0)
              '())
(check-expect (xexpr-content e1)
              '())
(check-expect (xexpr-content e2)
              '((action)))
(check-expect (xexpr-content e3)
              '((action)))
(check-expect (xexpr-content e4)
              '((action) (action)))

; Xexpr -> [List-of Attribute]
; Retrieve the list of attributes of xe.
(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-x
                 (first optional-loa+content)))
         (if (list-of-attributes? loa-or-x)
             loa-or-x
             '()))])))

(check-expect (xexpr-attr e0) '())
(check-expect (xexpr-attr e1) '((initial "X")))
(check-expect (xexpr-attr e2) '())
(check-expect (xexpr-attr e3) '())
(check-expect (xexpr-attr e4) '((initial "X")))

; [List-of Attribute] or Xexpr.v2 -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

(check-expect (list-of-attributes? '())
              #true)
(check-expect (list-of-attributes? '(action))
              #false)
(check-expect (list-of-attributes? a0)
              #true)