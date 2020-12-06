;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex254) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A [List-of ITEM] is one of:
;     – '()
;     – (cons ITEM [List-of ITEM])
; Interpretation:
;     A collection of ITEMs.

; A [NEList-of ITEM] is one of:
;     – (cons ITEM '())
;     – (cons ITEM [List-of ITEM])
; Interpretation:
;     A non-empty collection of ITEMs.

        	
(define-struct IR [name price])
; An IR is a structure:
;     (make-IR String Number)

; An Inventory is one of: 
;     – '()
;     – (cons IR Inventory)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; NOTE +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; When designing the function "sort-n" I got stuck and designed the following
; functions to get unstuck.
; These functions are not part of the actual exercise.

; END NOTE

; [List-of Number] -> [List-of Number]
; Return lon sorted in descending order.
(define (sort> lon)
  (cond
    [(empty? lon) '()]
    [(cons? lon)
     (insert> (first lon)
              (sort> (rest lon)))]))

(check-expect (sort> '())
              '())
(check-expect (sort> (list 506))
              (list 506))
(check-expect (sort> (list 506 162 911 690 478 121 107))
              (list 911 690 506 478 162 121 107))

; [List-of Number] -> [List-of Number]
; Return lon sorted in ascending order.
(define (sort< lon)
  (cond
    [(empty? lon) '()]
    [(cons? lon)
     (insert< (first lon)
              (sort< (rest lon)))]))

(check-expect (sort< '())
              '())
(check-expect (sort< (list 506))
              (list 506))
(check-expect (sort< (list 506 162 911 690 478 121 107))
              (list 107 121 162 478 506 690 911))

; [List-of ITEM] [ITEM ITEM -> Boolean] -> [List-of ITEM]
; Return lon sorted according to f.
(define (sort-abstract lon f)
  (cond
    [(empty? lon) '()]
    [(cons? lon)
     (insert (first lon)
             (sort-abstract (rest lon) f)
             f)]))

(check-expect (sort-abstract '() >)
              '())
(check-expect (sort-abstract (list 506) >)
              (list 506))
(check-expect (sort-abstract (list 506 162 911 690 478 121 107) >)
              (list 911 690 506 478 162 121 107))

(check-expect (sort-abstract '() <)
              '())
(check-expect (sort-abstract (list 506) <)
              (list 506))
(check-expect (sort-abstract (list 506 162 911 690 478 121 107) <)
              (list 107 121 162 478 506 690 911))

; Number [List-of Number] -> [List-of Number]
; Insert n into lon, which is sorted
; in descending order.
(define (insert> n lon)
  (cond
    [(empty? lon)
     (list n)]
    [(cons? lon)
     (if (> n (first lon))
         (cons n lon)
         (cons (first lon)
               (insert> n (rest lon))))]))

(check-expect (insert> 121 '())
              (list 121))
(check-expect (insert> 121 (list 911 690 506 478 162 121 107))
              (list 911 690 506 478 162 121 121 107))

; Number [List-of Number] -> [List-of Number]
; Insert n into lon, which is sorted
; in descending order.
(define (insert< n lon)
  (cond
    [(empty? lon)
     (list n)]
    [(cons? lon)
     (if (< n (first lon))
         (cons n lon)
         (cons (first lon)
               (insert< n (rest lon))))]))

(check-expect (insert< 121 '())
              (list 121))
(check-expect (insert< 121 (list 107 121 162 478 506 690 911))
              (list 107 121 121 162 478 506 690 911))

; ITEM [List-of ITEM] [ITEM ITEM -> Boolean] -> [List-of ITEM]
; Insert n into lon using f.
; The list lon must be sorted
; according to f.
(define (insert i loi f)
  (cond
    [(empty? loi)
     (list i)]
    [(cons? loi)
     (if (f i (first loi))
         (cons i loi)
         (cons (first loi)
               (insert i (rest loi) f)))]))

(check-expect (insert 121 '() >)
              (list 121))
(check-expect (insert 121 (list 911 690 506 478 162 121 107) >)
              (list 911 690 506 478 162 121 121 107))

(check-expect (insert 121 '() <)
              (list 121))
(check-expect (insert 121 (list 107 121 162 478 506 690 911) <)
              (list 107 121 121 162 478 506 690 911))

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 
; [List-of Number] [Number Number -> Boolean] -> [List-of Number]
; Return a sorted list of numbers
; according to the rule specified by f.
(define (sort-n lon f)
  (cond
    [(empty? lon) '()]
    [(cons? lon)
     (insert (first lon)
             (sort-n (rest lon) f)
             f)]))

(check-expect (sort-n '() >)
              '())
(check-expect (sort-n (list 871) >)
              (list 871))
(check-expect (sort-n (list 871 190) <)
              (list 190 871))
(check-expect (sort-n (list 871 190 295 224 983 97) >)
              (list 983 871 295 224 190 97))
(check-expect (sort-n (list 871 190 295 224 983 97) <)
              (list 97 190 224 295 871 983))

; [List-of String] [String String -> Boolean] -> [List-of String]
; Return a sorted list of strings
; according to the rule specified by f.
(define (sort-s los f)
  (cond
    [(empty? los) '()]
    [(cons? los)
     (insert (first los)
             (sort-s (rest los) f)
             f)]))

(check-expect (sort-s '() string<?)
              '())
(check-expect (sort-s (list "orange") string<?)
              (list "orange"))
(check-expect (sort-s (list "orange" "apple") string<?)
              (list "apple" "orange"))

(check-expect (sort-s (list "banana" "orange" "apple" "pineapple" "grape")
                      string>?)
              (list "pineapple" "orange" "grape" "banana" "apple"))

; [List-of ITEM] [ITEM ITEM -> Boolean] -> [List-of ITEM]
; Return a sorted list of ITEM
; according to the rule specified by f.
(define (sort-abs loi f)
  (cond
    [(empty? loi) '()]
    [(cons? loi)
     (insert (first loi)
             (sort-abs (rest loi) f)
             f)]))

(check-expect (sort-abs '() >)
              '())
(check-expect (sort-abs (list 871 190 295 224 983 97) <)
              (list 97 190 224 295 871 983))
(check-expect (sort-abs (list "banana" "orange" "apple" "pineapple" "grape")
                        string>?)
              (list "pineapple" "orange" "grape" "banana" "apple"))

; Inventory [IR IR -> Boolean] -> Inventory
; Sort inv according to the rule specified by f.

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; See exercise 187 for an example of sorting structures.

; Inventory [IR -> X] [IR IR -> Boolean] -> Inventory
; Sort inv using f and selecting the appropriate
; field using s.
(define (sort-inventory inv s f)
  (cond
    [(empty? inv) '()]
    [(cons? inv)
     (insert (first inv)
             (sort-inventory (rest inv) s f)
             (compare-ir ...))]))              ; -> see NOTE

; NOTE on "sort-inventory" -----------------------------------------------------

; This function is unfinished, since in the second cond-clause one would have
; to partially instantiate by passing it s and f. Something like that has not
; been dealt with up to now.

; ------------------------------------------------------------------------------

; IR IR -> Boolean
; Are the names of ir1 and ir2
; lexicographically sorted increasing?
(define (name<? ir1 ir2)
  (string<? (IR-name ir1) (IR-name ir2)))

(check-expect (name<? (make-IR "apple" 1)
                      (make-IR "banana" 2))
              #true)
(check-expect (name<? (make-IR "banana" 2)
                      (make-IR "apple" 1))
              #false)
(check-expect (name<? (make-IR "apple" 1)
                      (make-IR "apple" 1))
              #false)

; IR IR -> Boolean
; Are the prices of ir1 and ir2
; sorted increasing?
(define (price<? ir1 ir2)
  (< (IR-price ir1) (IR-price ir2)))

(check-expect (price<? (make-IR "apple" 1)
                      (make-IR "banana" 2))
              #true)
(check-expect (price<? (make-IR "banana" 2)
                      (make-IR "apple" 1))
              #false)
(check-expect (price<? (make-IR "apple" 1)
                      (make-IR "apple" 1))
              #false)

; IR IR [IR -> X] [Y Y -> Boolean] -> Boolean
; Compare ir1 and ir2 using f and selecting
; the appropriate fields with s.
(define (compare-ir ir1 ir2 s f)
  (f (s ir1) (s ir2)))

(check-expect (compare-ir (make-IR "apple" 1)
                          (make-IR "banana" 2)
                          IR-name
                          string<?)
              #true)
(check-expect (compare-ir (make-IR "banana" 2)
                          (make-IR "apple" 1)
                          IR-name
                          string<?)
              #false)

(check-expect (compare-ir (make-IR "apple" 1)
                          (make-IR "banana" 2)
                          IR-price
                          <)
              #true)
(check-expect (compare-ir (make-IR "banana" 2)
                          (make-IR "apple" 1)
                          IR-price
                          <)
              #false)
