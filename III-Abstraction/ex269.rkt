;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex269) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; A [List-of ITEM] is one of:
;     – '()
;     – (cons ITEM [List-of ITEM])
; Interpretation:
;     A list of ITEMs.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Number Inventory -> Inventory
; Produce a list of all those structures
; in inv whose sales price is below ua.
(define (eliminate-expensive ua inv)
  (local (; IR -> Boolean
          ; Is ir's sales price greater than ua?
          (define (expensive? ir)
            (> (IR-s-price ir) ua)))

    ; –IN –
    (filter expensive? inv)))

(check-expect (eliminate-expensive 1234 '())
              '())
(check-expect (eliminate-expensive
               0                                   
               (list (make-IR "car" "a car" 14999 20000)
                     (make-IR "TV" "a TV set" 500 650)
                     (make-IR "house" "a house" 350000 500000)))
              (list (make-IR "car" "a car" 14999 20000)
                    (make-IR "TV" "a TV set" 500 650)
                    (make-IR "house" "a house" 350000 500000)))
(check-expect (eliminate-expensive
               5000                                   
               (list (make-IR "car" "a car" 14999 20000)
                     (make-IR "TV" "a TV set" 500 650)
                     (make-IR "house" "a house" 350000 500000)))
              (list (make-IR "car" "a car" 14999 20000)
                    (make-IR "house" "a house" 350000 500000)))
(check-expect (eliminate-expensive
               50000                                  
               (list (make-IR "car" "a car" 14999 20000)
                     (make-IR "TV" "a TV set" 500 650)
                     (make-IR "house" "a house" 350000 500000)))
              (list (make-IR "house" "a house" 350000 500000)))

; String Inventory -> Inventory
; Produce a list of inventory records
; from inv that do not use the name ty.
(define (recall ty inv)
  (local (; IR -> Boolean
          ; Is the name of ir not ty?
          (define (not-named-ty? ir)
            (not (string=? (IR-name ir) ty))))
    
    ; – IN –
    (filter not-named-ty? inv)))

(check-expect (recall "house" '())
              '())
(check-expect (recall "house" (list (make-IR "house" "a house" 350000 500000)
                                    (make-IR "car" "a car" 14999 20000)
                                    (make-IR "TV" "a TV set" 500 650)
                                    (make-IR "house" "a house" 350000 500000)))
              (list (make-IR "car" "a car" 14999 20000)
                    (make-IR "TV" "a TV set" 500 650)))
(check-expect (recall "hammer" (list (make-IR "house" "a house" 350000 500000)
                                     (make-IR "car" "a car" 14999 20000)
                                     (make-IR "TV" "a TV set" 500 650)
                                     (make-IR "house" "a house" 350000 500000)))
              (list (make-IR "house" "a house" 350000 500000)
                    (make-IR "car" "a car" 14999 20000)
                    (make-IR "TV" "a TV set" 500 650)
                    (make-IR "house" "a house" 350000 500000)))

; [List-of String] [List-of String] -> [List-of String]
; Select all those strings from los2
; that are also on los1.
(define (selection los1 los2)
  (local (; String -> Boolean
          ; Is s a member of los1?
          (define (in-los1? s)
            (member? s los1)))

    ; – IN –
    (filter in-los1? los2)))

(check-expect (selection '() '())
              '())
(check-expect (selection
               (list "Fedelmid" "Feidlimid" "Cynbel" "Fáelán" "Nynniaw")
               '())
              '())
(check-expect (selection
               '()
               (list "Fedelmid" "Feidlimid" "Cynbel" "Fáelán" "Nynniaw"))
              '())
(check-expect (selection
               (list "Fedelmid" "Feidlimid" "Cynbel" "Fáelán" "Nynniaw")
               (list "Drustan" "Feidlimid" "Caomh" "Fáelán" "Caiside"))
              (list "Feidlimid" "Fáelán"))