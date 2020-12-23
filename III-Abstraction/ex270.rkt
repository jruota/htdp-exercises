;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex270) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A NatNumber is one of:
;     – 0
;     – (add1 NatNumber)
; Interpretation:
;     The natural numbers.

; A NatNumber+ is one of:
;     – 1
;     – (add1 NatNumber+)
; Interpretation:
;     The natural numbers without 0.

; A [List-of ITEM] is one of:
;     – '()
;     – (cons ITEM [List-of ITEM])
; Interpretation:
;     A list of ITEMs.

; A Matrix is one of: 
;  – (cons Row '())
;  – (cons Row Matrix)
; Constraint:
;     all rows in matrix are of the same length.

; An IM (identity matrix) is a square Matrix
; with ones on the main diagonal and
; zeros elsewhere.
; Constraint:
;     An IM has at least the dimension 1.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; NatNumber -> [List-of NatNumber]
; Create the list (list 0 ... (- n 1))
; for any natural number n.
(define (list-of-numbers n)
  (local (; [X] X -> X
          ; Do nothing.
          (define (do-nothing x)
            x))
    ; – IN –
    (build-list n do-nothing)))

(check-expect (list-of-numbers 0)
              '())
(check-expect (list-of-numbers 1)
              (list 0))
(check-expect (list-of-numbers 10)
              (list 0 1 2 3 4 5 6 7 8 9))

; NatNumber -> [List-of NatNumber+]
; Create the list (list 1 ... n)
; for any natural number n.
(define (list-of-positive-numbers n)
  (build-list n add1))

(check-expect (list-of-positive-numbers 0)
              '())
(check-expect (list-of-positive-numbers 1)
              (list 1))
(check-expect (list-of-positive-numbers 10)
              (list 1 2 3 4 5 6 7 8 9 10))

; NatNumber -> [List-of Number]
; Create the list (list 1 1/2 ... 1/n)
; for any natural number n.
(define (list-of-fractions n)
  (local (; NatNumber -> Number
          ; Add 1 to x and
          ; return 1/x.
          (define (add1-and-/ x)
            (/ 1 (add1 x))))
    ; – IN –
    (build-list n add1-and-/)))

(check-expect (list-of-fractions 0)
              '())
(check-expect (list-of-fractions 1)
              (list 1))
(check-within (list-of-fractions 10)
              (list 1 1/2 1/3 1/4 1/5 1/6 1/7 1/8 1/9 1/10)
              .1)

; NatNumber -> [List-of NatNumber]
; Create the list of the first n even numbers.
(define (list-of-even-numbers n)
  (local (; NatNumber -> NatNumber
          ; Multiply n by 2.
          (define (multiply-by-2 n)
            (* 2 n)))

    ; – IN –
    (build-list n multiply-by-2)))

(check-expect (list-of-even-numbers 0)
              '())
(check-expect (list-of-even-numbers 1)
              (list 0))
(check-expect (list-of-even-numbers 10)
              (list 0 2 4 6 8 10 12 14 16 18))

; NatNumber -> IM
; Return the identity matrix of size n.
(define (identityM n)
  (local (; NatNumber -> [List-of Number]
          ; Create a list of lenght n with
          ; a 1 at position x and 0s elsewhere.
          (define (make-identity-row x)
            (list-set (make-list n 0) x 1))

          ; List NatNumber Any -> List
          ; Return a list like lst but with
          ; value at index pos.
          ; If pos is greater or equal to (length lst),
          ; lst is returned unchanged.
          (define (list-set lst pos value)
            (cond
              [(empty? lst) '()]
              [else
               (cons
                (if (zero? pos)
                    value
                    (first lst))
                (list-set (rest lst) (sub1 pos) value))])))

    ; – IN –
    (build-list n make-identity-row)))

(check-expect (identityM 1)
              (list (list 1)))

(check-expect (identityM 2)
              (list (list 1 0)
                    (list 0 1)))

(check-expect (identityM 5)
              (list (list 1 0 0 0 0)
                    (list 0 1 0 0 0)
                    (list 0 0 1 0 0)
                    (list 0 0 0 1 0)
                    (list 0 0 0 0 1)))

; NatNumber [Number -> Number] -> [List-of Number]
; Tabulate f between n
; and 0 (inclusive).
(define (tabulate n f)
  (local ((define m (add1 n)))
    ; – IN –
    (reverse (build-list m f))))

(check-within (tabulate 0 sin)
              (list (sin 0))
              .1)
(check-within (tabulate 4 sqrt)
              (list (sqrt 4) (sqrt 3) (sqrt 2) (sqrt 1) (sqrt 0))
              .1)