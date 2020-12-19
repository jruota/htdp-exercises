;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex261) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An NNumber is one of:
;     – 0
;     – (add1 NNumber)
; Interpretation:
;     The natural numbers.

(define-struct IR
  [name price])
; An IR is a structure:
;   (make-IR String Number)

; An Inventory is one of: 
; – '()
; – (cons IR Inventory)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define TEST (list
              (make-IR "stamp-regular" 0.89)
              (make-IR "toy-car" 5.99)
              (make-IR "car-sticker" 0.99)
              (make-IR "house" 500000)
              (make-IR "car" 37500)
              (make-IR "pen" 0.85)))

(define RESULT (list
                (make-IR "stamp-regular" 0.89)
                (make-IR "car-sticker" 0.99)
                (make-IR "pen" 0.85)))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; for testing
; NNumber
; Create a list by copying l n times.
; If n is less than or equal to zero,
; the result is an empty list.
(define (copy-list n l)
  (cond
    [(<= n 0) '()]
    [else
     (append l
             (copy-list (sub1 n) l))]))

(check-expect (copy-list 0 TEST)
              '())

(check-expect (copy-list 3 TEST)
              (append TEST TEST TEST))

; Inventory -> Inventory
; creates an Inventory from an-inv for all
; those items that cost less than a dollar
(define (extract1 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else
     (cond
       [(<= (IR-price (first an-inv)) 1.0)
        (cons (first an-inv) (extract1 (rest an-inv)))]
       [else (extract1 (rest an-inv))])]))

(check-expect (extract1 '())
              '())

(check-expect (extract1 TEST)
              RESULT)

; Inventory -> Inventory
; creates an Inventory from an-inv for all
; those items that cost less than a dollar
(define (extract1.v2 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else
     (local
       ((define extract1-rest (extract1 (rest an-inv))))
       ; – IN –
       (cond
       [(<= (IR-price (first an-inv)) 1.0)
        (cons (first an-inv) extract1-rest)]
       [else extract1-rest]))]))

(check-expect (extract1.v2 '())
              '())

(check-expect (extract1.v2 TEST)
              RESULT)

;; Inventory -> Inventory
;; creates an Inventory from an-inv for all
;; those items that cost less than a dollar
;(define (extract1.v3 an-inv)
;  (cond
;    [(empty? an-inv) '()]
;    [else
;     (cond
;       (local
;         ((define extract1-rest (extract1 (rest an-inv))))
;         ; – IN –
;         [(<= (IR-price (first an-inv)) 1.0)
;          (cons (first an-inv) extract1-rest)]
;         [else extract1-rest]))]))
;
;(check-expect (extract1.v3 '())
;              '())
;
;(check-expect (extract1.v3 TEST)
;              RESULT)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Does this help increase the speed at which the function computes its result?
; Significantly? A little bit? Not at all?

; It does not increase the speed of the function at all. WHY?
