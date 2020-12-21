;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex262) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A NNumber (natural number) is one of
;     – 0
;     – (add1 NNumber)
; Interpretation:
;     The natural numbers.

; A NNumber+ (positive natural number) is one of
;     – 1
;     – (add1 NNumber+)
; Interpretation:
;     The natural numbers without zero.

; A Row is one of: 
;  – '() 
;  – (cons Number Row)

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

; NOTE -------------------------------------------------------------------------

; I first designed all functions after "; +++ ..." seperately, then combined
; them to make "indentityM" return the desired result. In a last step I decided
; which functions where generally useful and which were meaningful only in the
; context of "identityM". The latter were moved into the "local" expression
; in "identityM".

; END NOTE ---------------------------------------------------------------------

; NNumber+ -> IM
; Return an identity matrix of size n.
(define (identityM n)
  (local
    (; NNumber+ -> Row
     ; Create a non-empty row of size n
     ; with a 1 at position m
     ; and zeros elsewhere.
     (define (make-identity-row m n)
       (list-set (make-list n 0) m 1))

     ; NNumber NNumber+ -> IM
     ; Create a identity matrix of size n,
     ; always start with m equal to zero.
     ; This is a wrapper function and
     ; should not be used as a stand-alone.
     (define (make-identity-matrix m n)
       (cond
         [(>= m n) '()]
         [else
          (cons (make-identity-row m n)
                (make-identity-matrix (add1 m) n))])))
    ; – IN –
    (make-identity-matrix 0 n)))

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

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; NNumber NNumber+ -> IM
;; Create a identity matrix of size n,
;; always start with m equal to zero.
;; This is a wrapper function and
;; should not be used as a stand-alone.
;(define (make-identity-matrix m n)
;  (cond
;    [(>= m n) '()]
;    [else
;     (cons (make-identity-row m n)
;           (make-identity-matrix (add1 m) n))]))
;
;(check-expect (make-identity-matrix 0 1)
;              (list (list 1)))
;
;(check-expect (make-identity-matrix 0 2)
;              (list (list 1 0)
;                    (list 0 1)))
;
;(check-expect (make-identity-matrix 0 5)
;              (list (list 1 0 0 0 0)
;                    (list 0 1 0 0 0)
;                    (list 0 0 1 0 0)
;                    (list 0 0 0 1 0)
;                    (list 0 0 0 0 1)))
;
;; NNumber+ -> Row
;; Create a non-empty row of size n
;; with a 1 at position m
;; and zeros elsewhere.
;(define (make-identity-row m n)
;  (list-set (make-list n 0) m 1))
;
;(check-expect (make-identity-row 0 1)
;              (list 1))
;
;(check-expect (make-identity-row 2 5)
;              (list 0 0 1 0 0))
;(check-expect (make-identity-row 4 5)
;              (list 0 0 0 0 1))
;(check-expect (make-identity-row 5 5)
;              (list 0 0 0 0 0))

; NOTE -------------------------------------------------------------------------

; After designing this function I found "make-list" in the Racket reference and
; used that function instead.

; END NOTE ---------------------------------------------------------------------

; NNumber -> Row
; Create a list of length n
; with val as elements.
(define (make-row val n)
  (cond
    [(<= n 0) '()]
    [else
     (cons val
           (make-row val (sub1 n)))]))

(check-expect (make-row 0 0)
              '())
(check-expect (make-row 0 1)
              (list 0))
(check-expect (make-row 0 5)
              (list 0 0 0 0 0))

(check-expect (make-row "a" 5)
              (list "a" "a" "a" "a" "a"))

; ------------------------------------------------------------------------------

; List NNumber Any -> List
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
      (list-set (rest lst) (sub1 pos) value))]))

(check-expect (list-set (list 0 0 0 0 0) 0 1)
              (list 1 0 0 0 0))
(check-expect (list-set (list 0 0 0 0 0) 1 1)
              (list 0 1 0 0 0))
(check-expect (list-set (list 0 0 0 0 0) 2 1)
              (list 0 0 1 0 0))
(check-expect (list-set (list 0 0 0 0 0) 3 1)
              (list 0 0 0 1 0))
(check-expect (list-set (list 0 0 0 0 0) 4 1)
              (list 0 0 0 0 1))

(check-expect (list-set (list 0 0 0 0 0) 5 1)
              (list 0 0 0 0 0))
(check-expect (list-set (list 0 0 0 0 0) 6 1)
              (list 0 0 0 0 0))
(check-expect (list-set (list 0 0 0 0 0) -1 1)
              (list 0 0 0 0 0))