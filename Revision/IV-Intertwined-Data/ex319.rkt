;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex319) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; S-expr Symbol Symbol -> S-expr
; Return s with all occurrences of old replaced by new.
(define (substitute s old new)
  (cond
    [(atom? s)
     (substitute-atom s old new)]
    [else
     (substitute-sl s old new)]))

(check-expect (substitute 23 'old 'new) 23)
(check-expect (substitute "pineapple" 'old 'new) "pineapple")
(check-expect (substitute 'akarama 'old 'new) 'akarama)
(check-expect (substitute 'old 'old 'new) 'new)

(check-expect
 (substitute (list (list 'nold 'old (list 'nold 'old) 'old 23 "something")
                   'old "not-old" 'nold)
             'old
             'new)
 (list (list 'nold 'new (list 'nold 'new) 'new 23 "something")
       'new "not-old" 'nold))

; Atom -> Atom
; If a is a symbol and equal to old,
; replace it with new.
(define (substitute-atom a old new)
  (cond
    [(symbol? a)
     (if (symbol=? a old) new a)]
    [else
     a]))

(check-expect (substitute-atom 23 'old 'new) 23)
(check-expect (substitute-atom "hello" 'old 'new) "hello")
(check-expect (substitute-atom 'old 'old 'new) 'new)
(check-expect (substitute-atom 'not-old 'old 'new) 'not-old)

; SL -> SL
; Replace all occurences of old in sl with new.
(define (substitute-sl sl old new)
  (cond
    [(empty? sl) '()]
    [else
     (cons (substitute (first sl) old new)
           (substitute-sl (rest sl) old new))]))

(check-expect (substitute-sl '() 'old 'new) '())
(check-expect (substitute-sl (list 'nold 'nold 'nold 'old 'old 'nold) 'old 'new)
              (list 'nold 'nold 'nold 'new 'new 'nold))
(check-expect
 (substitute-sl (list (list 'nold 'old (list 'nold 'old) 'old 23 "something")
                      'old "not-old" 'nold)
                'old
                'new)
 (list (list 'nold 'new (list 'nold 'new) 'new 23 "something")
       'new "not-old" 'nold))

; from ex316.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Any -> Boolean
; Is a an Atom?
(define (atom? a)
  (or (number? a)
      (string? a)
      (symbol? a)))
