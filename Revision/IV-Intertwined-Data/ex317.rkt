;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex317) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An S-expr is one of: 
; – Atom
; – SL

; An Atom is one of: 
; – Number
; – String
; – Symbol

; An SL is one of: 
; – '()
; – (cons S-expr SL)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; S-expr Symbol -> N 
; counts all occurrences of sy in sexp 
(define (count sexp sy)
  (local (; Atom -> Number
          ; Return 1 if at is a symbol and equal to sy,
          ; 0 otherwise.
          (define (count-atom at)
            (cond
              [(symbol? at)
               (if (symbol=? at sy) 1 0)]
              [else 0]))

          ; S-expr -> N
          ; Count all occurrences of sy in sexp0
          (define (count-sl sexp0)
            (cond
              [(empty? sexp0) 0]
              [else
               (+ (count (first sexp0) sy)
                  (count-sl (rest sexp0)))])))
    ; – IN –
    (cond
      [(atom? sexp)
       (count-atom sexp)]
      [else
       (count-sl sexp)])))

(check-expect (count 'world 'world) 1)
(check-expect (count '() 'world) 0)
(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)

; from ex316.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Any -> Boolean
; Is a an Atom?
(define (atom? a)
  (or (number? a)
      (string? a)
      (symbol? a)))
