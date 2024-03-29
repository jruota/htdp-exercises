;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex412-challenge) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct inex [mantissa sign exponent])
; An Inex is a structure: 
;   (make-inex N99 S N99)

; An S is one of:
; – 1
; – -1

; An N99 is an N between 0 and 99 (inclusive).

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define EXPONENT-ERROR "exponents differ by more than one")
(define RANGE-ERROR "number is out of range")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Inex Inex -> Inex
; Add in1 and in2, whose exponents may not differ
; by more than one.
; Signal an error if the result is out of range.
(define (inex+ in1 in2)
  (local (; Inex Inex -> Inex
          ; Choose the right adding function depending on
          ; the relation of the exponents.
          (define (main inex1 inex2)
            (local ((define signed-exponent1
                      (* (inex-sign inex1) (inex-exponent inex1)))
                    (define signed-exponent2
                      (* (inex-sign inex2) (inex-exponent inex2))))
              ; – IN –
              (cond
                [(equal-exp? inex1 inex2)
                 (inex-add inex1 inex2)]
                [(exp-by-one? inex1 inex2)
                 (if (> signed-exponent1 signed-exponent2)
                     (inex-add (lower-exponent inex1) inex2)
                     (inex-add inex1 (lower-exponent inex2)))]
                [else (error EXPONENT-ERROR)])))

          ; Inex Inex -> Inex
          ; Add inex1 and inex2 and make sure that the result
          ; is an Inex itself, i.e. is within range.
          ; Signal an error if it is out of range.
          (define (inex-add inex1 inex2)
            (local ((define mantissa1 (inex-mantissa inex1))
                    (define mantissa2 (inex-mantissa inex2))
                    ; exponents are supposed to be equal
                    (define sign (inex-sign inex1))
                    (define exponent (inex-exponent inex1))

                    (define res
                      (format-mantissa
                       (make-inex (+ mantissa1 mantissa2)
                                  sign
                                  exponent)
                       99)))
              ; – IN –
              (if (> (inex-exponent res) 99)
                  (error RANGE-ERROR)
                  res)))

          ; Inex -> Inex
          ; Lower the exponent of inex by one.
          (define (lower-exponent inex)
            (local ((define mantissa-new (* 10 (inex-mantissa inex)))
                    (define sign (inex-sign inex))
                    (define exponent-new (- (inex-exponent inex) 1)))
              ; – IN –
              (if (and (>= sign 0) (< exponent-new 0))
                  (make-inex mantissa-new (* -1 (abs sign)) (abs exponent-new))
                  (make-inex mantissa-new sign (abs exponent-new)))))

          ; Inex Inex -> Boolean
          ; Are the exponents of inex1 and inex2 equal?
          (define (equal-exp? inex1 inex2)
            (= (* (inex-sign inex1) (inex-exponent inex1))
               (* (inex-sign inex2) (inex-exponent inex2))))

          ; Inex Inex -> Boolean
          ; Do the exponents of inex1 and inex2 differ
          ; by one?
          (define (exp-by-one? inex1 inex2)
            (= (abs (- (* (inex-sign inex1) (inex-exponent inex1))
                       (* (inex-sign inex2) (inex-exponent inex2))))
               1)))

    ; – IN –
    (main in1 in2)))

(check-expect (inex+ (create-inex 1 1 0) (create-inex 2 1 0))
              (create-inex 3 1 0))
(check-expect (inex+ (create-inex 55 1 0) (create-inex 55 1 0))
              (create-inex 11 1 1))
(check-expect (inex+ (create-inex 56 1 0) (create-inex 56 1 0))
              (create-inex 11 1 1))

(check-expect (inex+ (create-inex 1 1 0) (create-inex 1 -1 1))
              (create-inex 11 -1 1))
(check-expect (inex+ (create-inex 2 -1 0) (create-inex 5 -1 1))
              (create-inex 25 -1 1))
(check-expect (inex+ (create-inex 5 -1 4) (create-inex 2 -1 3))
              (create-inex 25 -1 4))

(check-error (inex+ (create-inex 45 1 99) (create-inex 55 1 99))
             RANGE-ERROR)

(check-error (inex+ (create-inex 99 -1 1) (create-inex 99 1 1))
             EXPONENT-ERROR)

; Inex N -> Inex
; Make sure that the mantissa of in
; is less than or equal to n by adapting
; the exponent of in.
(define (format-mantissa in n)
  (local (; Inex -> Inex
          ; Perform the actual change of mantissa
          ; and exponent.
          (define (inex-transform in0)
            (local ((define mantissa (inex-mantissa in0))
                    (define sign (inex-sign in0))
                    (define exponent (inex-exponent in0)))
              ; – IN –
              (if (> mantissa n)
                  (inex-transform
                   (make-inex (inexact->exact (round (/ mantissa 10)))
                              sign
                              (add1 exponent)))
                  (make-inex mantissa sign exponent)))))
    ; – IN –
    (inex-transform in)))

(check-expect (format-mantissa (make-inex 112 1 0) 99)
              (create-inex 11 1 1))
(check-expect (format-mantissa (make-inex 10000 -1 4) 99)
              (create-inex 10 -1 7))

; from figure 143 --------------------------------------------------------------

; N Number N -> Inex
; makes an instance of Inex after checking the arguments
(define (create-inex m s e)
  (cond
    [(and (<= 0 m 99) (<= 0 e 99) (or (= s 1) (= s -1)))
     (make-inex m s e)]
    [else (error "bad values given")]))