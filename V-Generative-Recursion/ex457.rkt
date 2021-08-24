;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex457) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number Number -> N
; Compute how many months it takes to double a given amount
; of money a when a savings account pays interest at a fixed
; rate r on a monthly basis.
(define (double-amount a r)
  (local (; N -> N
          ; How many months does it take to double
          ; amount a with an interest rate of r.
          (define (helper n)
            (cond
              [(>= (* a (expt (+ 1 r) n)) (* 2 a))
               n]
              [else
               (helper (add1 n))])))
    ; – IN –
    (helper 1)))

(check-within (double-amount 100 .02) 72/2 .9)
(check-within (double-amount 100 .03) 72/3 .9)
(check-within (double-amount 100 .04) 72/4 .9)
(check-within (double-amount 100 .05) 72/5 .9)
