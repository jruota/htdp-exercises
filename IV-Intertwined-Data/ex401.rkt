;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex401) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An S-expr (S-expression) is one of: 
; – Atom
; – [List-of S-expr]
; 
; An Atom is one of: 
; – Number
; – String
; – Symbol

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; NOTE -------------------------------------------------------------------------

; This is a mixture of the three cases for simultaneous processing.
; First, s2 is treated as if it were atomic. In the last (fourth) case, all
; possible cases are treated. It is only in the else that the case where s2
; is not a list of S-expr is treated.

; END NOTE ---------------------------------------------------------------------

; S-expr S-expr -> Boolean
; Are s1 and s2 equal?
(define (sexp=? s1 s2)
  (cond
    [(number? s1)
     (if (number? s2)
         (= s1 s2)
         #false)]
    [(string? s1)
     (if (string? s2)
         (string=? s1 s2)
         #false)]
    [(symbol? s1)
     (if (symbol? s2)
         (symbol=? s1 s2)
         #false)]
    [else
     (cond
       [(and (empty? s1) (empty? s2))
        #true]
       [(and (empty? s1) (cons? s2))
        #false]
       [(and (cons? s1) (empty? s2))
        #false]
       [(and (cons? s1) (cons? s2))
        (and (sexp=? (first s1) (first s2))
             (sexp=? (rest s1) (rest s2)))]
       [else #false])]))

(check-expect (sexp=? 3 4)
              #false)
(check-expect (sexp=? 3.14 3.14)
              #true)
(check-expect (sexp=? 2.71 'symbol)
              #false)

(check-expect (sexp=? "hello" "world")
              #false)
(check-expect (sexp=? "world" "world")
              #true)
(check-expect (sexp=? "hello world" 'symbol)
              #false)

(check-expect (sexp=? 'symbol1 'symbol2)
              #false)
(check-expect (sexp=? 'symbol 'symbol)
              #true)
(check-expect (sexp=? 'symbol "not a symbol")
              #false)

(check-expect (sexp=? '(1 "hello" (a b (c "world" 34)))
                      '(1 "hello" (a b (c "world" 43))))
              #false)

(check-expect (sexp=? '(1 "hello" (a b (c "world" 34)))
                      '(1 "hello" (a b (c "world" 34))))
              #true)
(check-expect (sexp=? '() '(1 "hello" (a b (c "world" 34))))
              #false)
(check-expect (sexp=? '(1 "hello" (a b (c "world" 34))) '())
              #false)
(check-expect (sexp=? '(1 "hello" (a b (c "world" 34)))
                      23)
              #false)