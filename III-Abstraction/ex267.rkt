;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex267) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define USD-per-EUR 1.06)

(define ABS-ZERO-K 0)
(define ABS-ZERO-F (- (* ABS-ZERO-K 9/5) 459.67))
(define ABS-ZERO-C (- ABS-ZERO-K 273.15))

; A Fahrenheit is a number f
; where f >= ABS-ZERO-F.
; Interpretation:
;     Temperatures on the Fahrenheit scale.

; A Celsius is a number c
; where c >= ABS-ZERO-C.
; Interpretation:
;     Temperatures on the Celsius scale.

; A Pair is a list of two number:
;     (list Number Number).

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; NOTE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; This function does not truncate or round, so that the results may have
; many decimal places.

; END NOTE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of Number] -> [List-of Number]
; Convert the US$ values in lon
; to EUR values.
(define (convert-euro lon)
  (local (; Number -> Number
          ; Convert the US$ value us
          ; to a EUR value.
          (define (us-to-eur us)
            (/ us USD-per-EUR)))
    ; – IN –
    (map us-to-eur lon)))

(check-expect (convert-euro '())
              '())
(check-within (convert-euro (list 0 1 2.45 34 174 6374.99))
              (list (/ 0 USD-per-EUR)
                    (/ 1 USD-per-EUR)
                    (/ 2.45 USD-per-EUR)
                    (/ 34 USD-per-EUR)
                    (/ 174 USD-per-EUR)
                    (/ 6374.99 USD-per-EUR))
              .1)

; [List-of Fahrenheit] -> [List-of Celsius]
; Convert the list of Fahrenheit measurements
; to a list of Celsius.
(define (convert-fc lof)
  (local (; Fahrenheit -> Celsius
          ; Convert the Fahrenheit measurement f
          ; to a Celsius value.
          (define (f-to-c f)
            (* (- f 32) 5/9)))
    ; – IN –
    (map f-to-c lof)))

(check-expect (convert-fc '())
              '())
(check-within (convert-fc (list -459.67
                                -320.4
                                -108.4
                                -40
                                31.9998
                                68
                                98.6
                                211.971))
              (list -273.15
                    -195.8
                    -78
                    -40
                    0.0001
                    20
                    37
                    99.9839)
              .1)

; [List-of Posn] -> [List-of Pair]
; Translate a list of Posns into a
; list of lists of pairs of numbers.
(define (translate lop)
  (local (; Posn -> Pair
          ; Translate the posn p to
          ; a pair (list of two numbers).
          (define (posn-to-pair p)
            (list (posn-x p) (posn-y p))))
    (map posn-to-pair lop)))

(check-expect (translate '())
              '())
(check-expect (translate (list (make-posn 419 643)
                               (make-posn 945 546)
                               (make-posn 355 29)))
              (list (list 419 643)
                    (list 945 546)
                    (list 355 29)))