;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex319) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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
; Replace all accurences of old in
; sexpr with new.
(define (substitute sexpr old new)
  (local (; Atom -> Atom
          ; Replace at with new if it is
          ; equal to old, return at otherwise.
          (define (atom-substitute at)
            (cond
              [(and (symbol? at)
                    (symbol=? at old))
               new]
              [else at]))

          ; SL -> SL
          ; Replace all accurences of old in
          ; sl with new.
          (define (sl-substitute sl)
            (cond
              [(empty? sl) '()]
              [else
               (cons (substitute (first sl) old new)
                     (sl-substitute (rest sl)))])))
    ; – IN –
    (cond
      [(atom? sexpr)
       (atom-substitute sexpr)]
      [else
       (sl-substitute sexpr)])))

(check-expect (substitute '() 'hello 'world)
              '())
(check-expect (substitute '(banana hello world + 66 hello) 'hello 'world)
              '(banana world world + 66 world))
(check-expect (substitute '(world) 'hello 'world)
              '(world))
(check-expect (substitute '((world) hello) 'hello 'world)
              '((world) world))
 (list (list (list 'world) 'hello) 'hello)
(check-expect (substitute '(((world) hello) hello) 'hello 'world)
              '(((world) world) world))
; (list 4 "banana" '+ '() (list (list (list 'world) 'hello) 'hello))
(check-expect (substitute '(4 "banana" + () (((world) hello) hello))
                          'world
                          'hello)
              '(4 "banana" + () (((hello) hello) hello)))

; SL Symbol Symbol -> SL
; Replace all accurences of old in
; sl with new.
(define (sl-substitute sl old new)
  (cond
    [(empty? sl) '()]
    [else
     (cons (substitute (first sl) old new)
           (sl-substitute (rest sl) old new))]))

(check-expect (substitute 'hello 'hello 'world)
              'world)
(check-expect (substitute 'world 'hello 'world)
              'world)
(check-expect (sl-substitute '() 'hello 'world)
              '())
(check-expect (sl-substitute '(world) 'hello 'world)
              '(world))
(check-expect (sl-substitute '(hello) 'hello 'world)
              '(world))
(check-expect (sl-substitute '((world) hello) 'hello 'world)
              '((world) world))
(check-expect (sl-substitute '((world) hello) 'world 'hello)
              '((hello) hello))
(check-expect (sl-substitute '(((world) hello) hello) 'hello 'world)
              '(((world) world) world))
(check-expect (sl-substitute '(((world) hello) hello) 'world 'hello)
              '(((hello) hello) hello))
(check-expect (sl-substitute '(hello (hello (world))) 'hello 'world)
              '(world (world (world))))

; Atom -> Atom
; Replace at with new if it is
; equal to old, return at otherwise.
(define (atom-substitute at old new)
  (cond
    [(and (symbol? at)
          (symbol=? at old))
     new]
    [else at]))

(check-expect (atom-substitute 42 'hello 'world)
              42)
(check-expect (atom-substitute "hello" 'hello 'world)
              "hello")
(check-expect (atom-substitute 'world 'hello 'world)
              'world)
(check-expect (atom-substitute 'hello 'hello 'world)
              'world)

; from ex316.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Any -> Boolean
; Is any of type Atom?
(define (atom? any)
  (or (number? any)
      (string? any)
      (symbol? any)))

;(check-expect (atom? 0) #true)
;(check-expect (atom? #true) #false)
;(check-expect (atom? empty-image) #false)
;(check-expect (atom? "hello world") #true)
;(check-expect (atom? (make-posn 1 2)) #false)
;(check-expect (atom? 'symbol) #true)
;(check-expect (atom? '()) #false)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define test '(((world) hello) hello))
(define f (first test))    ; (list (list 'world) 'hello)
(define r (rest test))     ; (list 'hello)

(define ff (first f))      ; (list 'world)
(define fr (rest f))       ; (list 'hello)
(define fff (first ff))    ; 'world
(define ffr (rest ff))     ; '()
(define frf (first fr))    ; 'hello
(define frr (rest fr))     ; '()

(define rf (first r))      ; 'hello
(define rr (rest r))       ; '()

(substitute rf 'hello 'world)
(substitute frf 'hello 'world)
(substitute fff 'hello 'world)

(substitute rr 'hello 'world)
(substitute frr 'hello 'world)
(substitute ffr 'hello 'world)

(substitute fr 'hello 'world)
(substitute ff 'hello 'world)
(substitute r 'hello 'world)

(substitute f 'hello 'world)

(cons (substitute f 'hello 'world) (substitute r 'hello 'world))
(substitute test 'hello 'world)

; ------------------------------------------------------------------------------

(sl-substitute rr 'hello 'world)
(sl-substitute frr 'hello 'world)
(sl-substitute ffr 'hello 'world)

(sl-substitute fr 'hello 'world)
(sl-substitute ff 'hello 'world)
(sl-substitute r 'hello 'world)

(sl-substitute f 'hello 'world)

(cons (substitute f 'hello 'world) (sl-substitute r 'hello 'world))
(sl-substitute test 'hello 'world)
