;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex286) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
(define-struct IR [name desc acq-price s-price])
; An IR is a structure:
;   (make-IR String String Number Number)
; Interpretation:
;     An inventory record specifying the name
;     of an item, its description, the acquisition
;     price, and the recommended sales price.

; An Inventory is one of: 
; – '()
; – (cons IR Inventory)
; Interpretation:
;     A list of inventory records.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [Number Number -> Boolean] Inventory -> Inventory
; Sort inv by the difference between the acquisition
; and sales price of its elements using f.
(define (sort-inventory inv f)
  (sort inv
        (lambda (x y)
          (f (- (IR-s-price x) (IR-acq-price y))
             (- (IR-s-price y) (IR-acq-price y))))))

(check-expect (sort-inventory '() >)
              '())
(check-expect (sort-inventory (list (make-IR "car" "a car" 14999 20000)
                                    (make-IR "TV" "a TV set" 500 650)
                                    (make-IR "house" "a house" 350000 500000))
                              >)
              (list (make-IR "house" "a house" 350000 500000)
                    (make-IR "car" "a car" 14999 20000)
                    (make-IR "TV" "a TV set" 500 650)))
(check-expect (sort-inventory (list (make-IR "car" "a car" 14999 20000)
                                    (make-IR "TV" "a TV set" 500 650)
                                    (make-IR "house" "a house" 350000 500000))
                              <)
              (list (make-IR "TV" "a TV set" 500 650)
                    (make-IR "car" "a car" 14999 20000)
                    (make-IR "house" "a house" 350000 500000)))