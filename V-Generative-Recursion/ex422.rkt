;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex422) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An N is one of:
; – 0
; – (add1 N)
; Interpretation:
;     The natural numbers.

; An N+ is one of:
; – 1
; – (add1 N)
; Interpretation:
;     The natural numbers without zero.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define INPUT-ERROR "expects an integer greater than 0")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of X] N+ -> [List-of [List-of X]]
; Return a list of list chunks of size n.
(define (list->chunks l n)
  (local (; [List-of X] -> [List-of [List-of X]]
          ; Perform the actual work.
          (define (main l)
            (cond
              [(empty? l) '()]
              [else (cons (take l n) (list->chunks (drop l n) n))])))
    ; – IN –
    (cond
      [(and (integer? n) (> n 0)) (main l)]
      [else (error INPUT-ERROR)])))

(check-error (list->chunks (list 1 "hello" 'world 77 (make-posn 1 2)) 0)
             INPUT-ERROR)
(check-expect (list->chunks '() 3)
             '())

(check-expect (list->chunks (list 1 "hello" 'world 77 (make-posn 1 2)) 1)
              (list (list 1)
                    (list "hello")
                    (list 'world)
                    (list 77)
                    (list (make-posn 1 2))))
(check-expect (list->chunks (list 1 "hello" 'world 77 (make-posn 1 2)) 2)
              (list (list 1 "hello") (list 'world 77) (list (make-posn 1 2))))
(check-expect (list->chunks (list 1 "hello" 'world 77 (make-posn 1 2)) 3)
              (list (list 1 "hello" 'world) (list 77 (make-posn 1 2))))
(check-expect (list->chunks (list 1 "hello" 'world 77 (make-posn 1 2)) 6)
              (list (list 1 "hello" 'world 77 (make-posn 1 2))))

; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea take n items and drop n at a time
(define (bundle s n)
  (cond
    [(empty? s) '()]
    [else
     (map implode (list->chunks s n))]))

(check-expect (bundle (explode "abcdefg") 3)
              (list "abc" "def" "g"))
(check-expect (bundle '() 3) '())

; from figure 147 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))
 
; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))