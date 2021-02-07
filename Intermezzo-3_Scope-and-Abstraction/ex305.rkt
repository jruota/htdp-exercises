;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex305) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define USD-per-EUR 1.06)

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
    (for/list ([us-value lon])
      (us-to-eur us-value))))

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