;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex328) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; S-expr Symbol Atom -> S-expr
; replaces all occurrences of old in sexp with new 
(define (substitute sexp old new)
  (local (; S-expr -> S-expr
          (define (for-sexp sexp)
            (cond
              [(atom? sexp)
               (if (equal? sexp old) new sexp)]
              [else
               (map for-sexp sexp)])))
    (for-sexp sexp)))

(check-expect (substitute 2.714 'world 'apple)
              2.714)
(check-expect (substitute "world" 'world 'apple)
              "world")
(check-expect (substitute 'world 'world 'apple)
              'apple)
(check-expect (substitute '() 'world 'apple)
              '())
(check-expect (substitute '((((((hello) world) hello) world) hello) world)
                          'world
                          'apple)
              '((((((hello) apple) hello) apple) hello) apple))

 
(check-expect (substitute '(((world) bye) bye) 'bye '42)
              '(((world) 42) 42))

; S-expr Symbol Atom -> S-expr
; replaces all occurrences of old in sexp with new 
(define (substitute-simplified sexp old new)
  (cond
    [(atom? sexp) (if (equal? sexp old) new sexp)]
    [else
     (map (lambda (s) (substitute-simplified s old new)) sexp)]))

(check-expect (substitute-simplified 2.714 'world 'apple)
              2.714)
(check-expect (substitute-simplified "world" 'world 'apple)
              "world")
(check-expect (substitute-simplified 'world 'world 'apple)
              'apple)
(check-expect (substitute-simplified '() 'world 'apple)
              '())
(check-expect (substitute-simplified
               '((((((hello) world) hello) world) hello) world)
               'world
               'apple)
              '((((((hello) apple) hello) apple) hello) apple))

 
(check-expect (substitute-simplified '(((world) bye) bye) 'bye '42)
              '(((world) 42) 42))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Explain why we had to use lambda for this last simplification.

; From the documentation of "map":

;    Applies proc to the elements of the lsts from the first elements
;    to the last. The proc argument must accept the same number of
;    arguments as the number of supplied lsts, and all lsts must have
;    the same number of elements. The result is a list containing each
;    result of proc in order.

; Since "substitute-simplified" is a function with three arguments and
; needs to call itself in the else-clause, but "map" needs a one argument
; function as its first argument (there is only the list sexp), one needs
; "lambda" to make a one argument wrapper around "substitute-simplified".
; The other arguments are passed as bound arguments (from the surrounding
; "substitute-simplified").

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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