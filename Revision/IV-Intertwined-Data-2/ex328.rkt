;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex328) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; ##############################################################################
; Stop! Explain why we had to use lambda for this last simplification.

;     While map only accepts one argument functions, substitute is a three
;     argument function. The lambda is a wrapper to bridge this difference
;     in the number of arguments.

; ##############################################################################

; S-expr Symbol Atom -> S-expr
; replaces all occurrences of old in sexp with new 
(define (substitute sexp old new)
  (cond
    [(atom? sexp) (if (equal? sexp old) new sexp)]
    [else
     (map (lambda (s) (substitute s old new)) sexp)]))
 
(check-expect (substitute '(((world) bye) bye) 'bye '42)
              '(((world) 42) 42))

(check-expect (substitute '() 'hello 'world)
              '())
(check-expect (substitute '(banana hello world + 66 hello) 'hello 'world)
              '(banana world world + 66 world))
(check-expect (substitute '(world) 'hello 'world)
              '(world))
(check-expect (substitute '((world) hello) 'hello 'world)
              '((world) world))
; (list (list (list 'world) 'hello) 'hello)
(check-expect (substitute '(((world) hello) hello) 'hello 'world)
              '(((world) world) world))
; (list 4 "banana" '+ '() (list (list (list 'world) 'hello) 'hello))
(check-expect (substitute '(4 "banana" + () (((world) hello) hello))
                          'world
                          'hello)
              '(4 "banana" + () (((hello) hello) hello)))

; from ex316.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Any -> Boolean
; Is any of type Atom?
(define (atom? any)
  (or (number? any)
      (string? any)
      (symbol? any)))