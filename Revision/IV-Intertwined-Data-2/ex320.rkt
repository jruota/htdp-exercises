;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex320) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
; Count all occurrences of sy in sexp.
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

(check-expect (count 'world 'world) 1)
(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)

; ##############################################################################

; For the first step, reformulate the data definition for S-expr so that the
; first clause of the first data definition is expanded into the three clauses
; of Atom and the second data definition uses the List-of abstraction.
; Redesign the count function for this data definition.

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An S-expr is one of: 
; – Number
; – String
; – Symbol
; – SL
 
; An SL is a [List-of S-expr].

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; S-expr Symbol -> N 
; Count all occurrences of sy in sexp.
(define (count.v2 sexp sy)
  (local (; SL -> N
          ; Count all occurrences of sy in sl.
          (define (sl-count sl)
            (cond
              [(empty? sl) 0]
              [else
               (+ (count.v2 (first sl) sy)
                  (sl-count (rest sl)))])))
    ; – IN –
    (cond
      [(number? sexp) 0]
      [(string? sexp) 0]
      [(symbol? sexp)
       (if (symbol=? sexp sy) 1 0)]
      [else
       (sl-count sexp)])))

(check-expect (count.v2 1 'world) 0)
(check-expect (count.v2 "world" 'world) 0)
(check-expect (count.v2 'world 'world) 1)
(check-expect (count.v2 'world 'hello) 0)
(check-expect (count.v2 '(world hello) 'hello) 1)
(check-expect (count.v2 '(((world) hello) hello) 'hello) 2)

; ##############################################################################

; For the second step, Integrate the data definition of SL into the one
; for S-expr. Simplify count again. Hint Use lambda.

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An S-expr is one of: 
; – Number
; – String
; – Symbol
; – [List-of S-expr]

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; S-expr Symbol -> N 
; Count all occurrences of sy in sexp.
(define (count.v3 sexp sy)
  (cond
    [(number? sexp) 0]
    [(string? sexp) 0]
    [(symbol? sexp)
     (if (symbol=? sexp sy) 1 0)]
    [else
     (foldl (lambda (s acc) (+ (count.v3 s sy) acc)) 0 sexp)]))

(check-expect (count.v3 1 'world) 0)
(check-expect (count.v3 "world" 'world) 0)
(check-expect (count.v3 'world 'world) 1)
(check-expect (count.v3 'world 'hello) 0)
(check-expect (count.v3 '(world hello) 'hello) 1)
(check-expect (count.v3 '(((world) hello) hello) 'hello) 2)

; from ex316.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Any -> Boolean
; Is any of type Atom?
(define (atom? any)
  (or (number? any)
      (string? any)
      (symbol? any)))

(check-expect (atom? 0) #true)
(check-expect (atom? #true) #false)
(check-expect (atom? empty-image) #false)
(check-expect (atom? "hello world") #true)
(check-expect (atom? (make-posn 1 2)) #false)
(check-expect (atom? 'symbol) #true)
(check-expect (atom? '()) #false)
