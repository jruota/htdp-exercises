;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex319) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; S-expr Symbol Symbol -> S-expr
; Replace all occurrences
; of old in sexpr with new.
(define (substitute sexpr old new)
  (local (; SL -> SL
          ; Replace all occurrences
          ; of old in sl with new.
          (define (sl-substitute sl)
            (cond
              [(empty? sl) '()]
              [else
               (cons (substitute (first sl) old new)
                     (sl-substitute (rest sl)))]))

          ; Atom -> Atom
          ; If sexpr is equal to old,
          ; substitute it with new.
          (define (atom-substitute sexpr)
            ; signature assumes only symbols are passed for old and new,
            ; therefore there is no need to check whether sexpr is a symbol
            (if (equal? sexpr old)
                new
                sexpr)))
    ; – IN –
    (cond
      [(atom? sexpr)
       (atom-substitute sexpr)]
      [else
       (sl-substitute sexpr)])))

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

;; S-expr Symbol Symbol -> S-expr
;; Replace all occurrences
;; of old in sexpr with new.
;(define (substitute sexpr old new)
;  (cond
;    [(atom? sexpr)    ; a seperate function would be overkill here
;     (if (equal? sexpr old)
;         new
;         sexpr)]
;    [else
;     (sl-substitute sexpr old new)]))
;
;(check-expect (substitute 2.714 'world 'apple)
;              2.714)
;(check-expect (substitute "world" 'world 'apple)
;              "world")
;(check-expect (substitute 'world 'world 'apple)
;              'apple)
;(check-expect (substitute '() 'world 'apple)
;              '())
;(check-expect (substitute '((((((hello) world) hello) world) hello) world)
;                          'world
;                          'apple)
;              '((((((hello) apple) hello) apple) hello) apple))
;
;; SL Symbol Symbol
;; Replace all occurrences
;; of old in sl with new.
;(define (sl-substitute sl old new)
;  (cond
;    [(empty? sl) '()]
;    [else
;     (cons (substitute (first sl) old new)
;           (sl-substitute (rest sl) old new))]))
;
;(check-expect (substitute '() 'world 'apple)
;              '())
;(check-expect (substitute '((((((hello) world) hello) world) hello) world)
;                          'world
;                          'apple)
;              '((((((hello) apple) hello) apple) hello) apple))

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