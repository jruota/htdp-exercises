;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex433) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define INPUT-ERROR "n must be greater than 0 with non-empty list")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea take n items and drop n at a time
; Termination:
;     (bundle s 0) loops infinitely unless s is '().
(define (bundle s n)
  (local (; [List-of 1String] -> [List-of String]
          ; Do the actual bundling.
          (define (main lo1s)
            (cond
              [(empty? lo1s) '()]
              [else
               (cons (implode (take lo1s n)) (bundle (drop lo1s n) n))])))
    ; – IN –
    (if (and (cons? s) (zero? n))
        (error INPUT-ERROR)
        (main s))))

(check-expect (bundle '() 0) '())

(check-error (bundle (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j") 0)
             INPUT-ERROR)

(check-expect (bundle (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j") 3)
              (list "abc" "def" "ghi" "j"))

; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))

(check-expect (take '() 0) '())
(check-expect (take (range 1 11 1) 0) '())
(check-expect (take '() 3) '())
(check-expect (take (range 1 11 1) 3) (range 1 4 1))
 
; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))

(check-expect (drop '() 0) '())
(check-expect (drop (range 1 11 1) 0) (range 1 11 1))
(check-expect (drop '() 3) '())
(check-expect (drop (range 1 11 1) 3) (range 4 11 1))