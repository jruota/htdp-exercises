;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex320) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/abstraction)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A [List-of X] is one of:
; – '()
; – (cons X [List-of X])

; An S-expr is one of: 
; – Number
; – String
; – Symbol 
; – [List-of S-expr]

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; S-expr Symbol -> N 
; Count all occurrences of sy in sexp. 
(define (count sexp sy)
  (local (; S-expr -> S-expr
          ; Count all occurrences of sy in sexp.
          (define (symbol-count sexp)
            (if (symbol=? sexp sy)
                1
                0))

          ; S-expr -> S-expr
          ; Count all occurrences of sy in sexp.
          (define (sl-count sexp)
            (cond
              [(empty? sexp) 0]
              [else
               (+ (count (first sexp) sy)
                  (sl-count (rest sexp)))])))
    ; – IN –
    (cond
      [(number? sexp) 0]
      [(string? sexp) 0]
      [(symbol? sexp)
       (symbol-count sexp)]
      [else
       (sl-count sexp)])))

(check-expect (count 44 'hello) 0)
(check-expect (count "ananas" 'hello) 0)
(check-expect (count 'world 'hello) 0)
(check-expect (count 'hello 'hello) 1)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '((((world) hello) hello) 27) 'hello) 2)

; S-expr Symbol -> N 
; Count all occurrences of sy in sexp. 
(define (count.v2 sexp sy)
  (cond
    [(number? sexp) 0]
    [(string? sexp) 0]
    [(symbol? sexp)
     (if (symbol=? sexp sy)
         1
         0)]
    [else
     (for/sum ([s sexp])
       (count.v2 s sy))]))

(check-expect (count.v2 44 'hello) 0)
(check-expect (count.v2 "ananas" 'hello) 0)
(check-expect (count.v2 'world 'hello) 0)
(check-expect (count.v2 'hello 'hello) 1)
(check-expect (count.v2 '(world hello) 'hello) 1)
(check-expect (count.v2 '((((world) hello) hello) 27) 'hello) 2)

; S-expr Symbol -> N
; Count all occurences of sy in sexp.
(define (count.v3 sexp sy)
  (cond
    [(number? sexp) 0]
    [(string? sexp) 0]
    [(symbol? sexp)
     (if (symbol=? sexp sy)
         1
         0)]
    [(empty? sexp) 0]
    [else
     (+ (count.v3 (first sexp) sy)
        (count.v3 (rest sexp) sy))]))
    
(check-expect (count.v3 44 'hello) 0)
(check-expect (count.v3 "ananas" 'hello) 0)
(check-expect (count.v3 'world 'hello) 0)
(check-expect (count.v3 'hello 'hello) 1)
(check-expect (count.v3 '(world hello) 'hello) 1)
(check-expect (count.v3 '((((world) hello) hello) 27) 'hello) 2)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; ##############################################################################
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Integrate the data definition of SL into the one for S-expr. Simplify count
; again. Consider using lambda.

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A [List-of X] is one of:
; – '()
; – (cons X [List-of X])

; An S-expr is one of: 
; – Number
; – String
; – Symbol
; – [List-of S-expr]

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; S-expr Symbol -> N
; Count all occurences of sy in sexp.
(define (count.v4 sexp sy)
  (cond
    [(number? sexp) 0]
    [(string? sexp) 0]
    [(symbol? sexp)
     (if (symbol=? sexp sy)
         1
         0)]
    [else
     (foldl (lambda (x y) (+ (count.v4 x sy) y)) 0 sexp)])) 
    

(check-expect (count.v4 44 'hello) 0)
(check-expect (count.v4 "ananas" 'hello) 0)
(check-expect (count.v4 'world 'hello) 0)
(check-expect (count.v4 'hello 'hello) 1)
(check-expect (count.v4 '(world hello) 'hello) 1)
(check-expect (count.v4 '((((world) hello) hello) 27) 'hello) 2)