;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex317) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An S-expr is one of: 
; – Atom
; – SL
 
; An SL is one of: 
; – '()
; – (cons S-expr SL)

; An Atom is one of: 
; – Number
; – String
; – Symbol 

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; S-expr Symbol -> N 
; counts all occurrences of sy in sexp 
(define (count sexp sy)
  (local (; SL Symbol -> N
          ; counts all occurrences of sy in sl
          (define (count-sl sl)
            (cond
              [(empty? sl) 0]
              [else
               (+ (count (first sl) sy) (count-sl (rest sl)))]))

          ; Atom Symbol -> N
          ; counts all occurrences of sy in at
          (define (count-atom at)
            (cond
              [(number? at) 0]
              [(string? at) 0]
              [(symbol? at) (if (symbol=? at sy) 1 0)])))
    ; – IN –
    (cond
      [(atom? sexp) (count-atom sexp)]
      [else (count-sl sexp)])))

(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)

; Any -> Boolean
; Is a of type atom?
(define (atom? a)
  (or (number? a)
      (string? a)
      (symbol? a)))

(check-expect (atom? 4.321)
              #true)
(check-expect (atom? "hello")
              #true)
(check-expect (atom? '+)
              #true)

(check-expect (atom? (make-posn 1 2))
              #false)
(check-expect (atom? empty-image)
              #false)
(check-expect (atom? '())
              #false)