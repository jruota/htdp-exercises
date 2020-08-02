;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex164) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; SOURCE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; https://www.xe.com/currencycharts/?from=USD&to=EUR

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A USD is a PositiveNumber.
; Interpretation:
;     An amount in US Dollars.

; A EUR is a PositiveNumber.
; Interpretation:
;     An amount in Euro.

; A USDAmounts is one of:
; – '()
; – (cons USD USDAmounts)
; Interpretation:
;     A list with amounts in US Dollars.

; A EURAmounts is one of:
; – '()
; – (cons EUR EURAmounts)
; Interpretation:
;     A list with amounts in Euro.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define EX-RATE 0.84939)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; PositiveNumber USDAmounts -> EURAmounts
; Convert the values in usda from US$
; to € using ex as the exchange rate.
(define (convert-euro* ex usda)
  (cond
    [(empty? usda) '()]
    [(cons? usda)
     (cons (* ex (first usda))
           (convert-euro* ex (rest usda)))]))

(check-expect (convert-euro* 1 '())
              '())
(check-expect (convert-euro* EX-RATE (cons 100 (cons 579 (cons 1 '()))))
              (cons (* EX-RATE 100)
                    (cons (* EX-RATE 579)
                          (cons (* EX-RATE 1) '()))))
(check-expect (convert-euro* 2 (cons 100 (cons 579 (cons 1 '()))))
              (cons 200
                    (cons 1158
                          (cons 2 '()))))

; USDAmounts -> EURAmounts
; Convert the values in usda from US$
; to €.
(define (convert-euro usda)
  (cond
    [(empty? usda) '()]
    [(cons? usda)
     (cons (usd->eur (first usda))
           (convert-euro (rest usda)))]))

(check-expect (convert-euro '())
              '())
(check-expect (convert-euro (cons 100 (cons 579 (cons 1 '()))))
              (cons (usd->eur 100)
                    (cons (usd->eur 579)
                          (cons (usd->eur 1) '()))))

; USD -> EUR
; Convert u from US$ to €.
(define (usd->eur u)
  (* EX-RATE u))

(check-expect (usd->eur 0)
              0)
(check-expect (usd->eur 34)
              (* EX-RATE 34))