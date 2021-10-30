;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex430) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; NOTE -------------------------------------------------------------------------

; The functions use "(rest lox)" and thus are not completely
; generatively recursive, as this is structurally recursive. Thus the functions
; use a mixture of generative and structural recursion.

; END NOTE ---------------------------------------------------------------------

; [List-of X] [X X -> Boolean] [List-of X]
; Sort lox from least to greatest.
(define (quick-sort lox)
  (cond
    [(empty? lox) '()]
    [(empty? (rest lox)) lox]
    [else
     (local ((define pivot (first lox)))
       ; – IN –
       (append (quick-sort (filter (lambda (x) (< x pivot)) lox))
               (list pivot)
               (quick-sort
                ; note the use of "not" and "rest";
                ; this also takes care of possible duplicates of the pivot,
                ; because the "not" turns the "<" into ">=" and only the
                ; first occurence of the pivot is removed
                (filter (lambda (x) (not (< x pivot))) (rest lox)))))]))

(check-expect (quick-sort '()) '())
(check-expect (quick-sort (list 23)) (list 23))
(check-expect (quick-sort (list 11 8 14 7)) (list 7 8 11 14))
(check-expect (quick-sort (list 11 9 2 18 12 14 4 1))
              (list 1 2 4 9 11 12 14 18))
(check-expect (quick-sort (list 11 14 9 4 2 9 18 1 12 18 11 2 12 14 4 1))
              (list 1 1 2 2 4 4 9 9 11 11 12 12 14 14 18 18))

; [List-of X] [X X -> Boolean] [List-of X]
; Sort lox according to cmp.
(define (quick-sort-abstract lox cmp)
  (cond
    [(empty? lox) '()]
    [(empty? (rest lox)) lox]
    [else
     (local ((define pivot (first lox)))
       ; – IN –
       ; note "(rest lox)" here; this allows the use of non strict comparison
       ; functions, as this removes one occurence of the pivot
       (append (quick-sort-abstract
                (filter (lambda (x) (cmp x pivot)) (rest lox)) cmp)
               (list pivot)
               (quick-sort-abstract
                ; note the use of "not" and "rest"
                (filter (lambda (x) (not (cmp x pivot))) (rest lox))
                cmp)))]))

(check-expect (quick-sort-abstract '() <) '())
(check-expect (quick-sort-abstract (list 23) <) (list 23))
(check-expect (quick-sort-abstract (list 11 8 14 7) <) (list 7 8 11 14))
(check-expect (quick-sort-abstract (list 11 9 2 18 12 14 4 1) <)
              (list 1 2 4 9 11 12 14 18))
(check-expect
 (quick-sort-abstract (list 11 14 9 4 2 9 18 1 12 18 11 2 12 14 4 1) <)
 (list 1 1 2 2 4 4 9 9 11 11 12 12 14 14 18 18))
(check-expect
 (quick-sort-abstract (list 11 14 9 4 2 9 18 1 12 18 11 2 12 14 4 1) <=)
 (list 1 1 2 2 4 4 9 9 11 11 12 12 14 14 18 18))

(check-expect (quick-sort-abstract '() >) '())
(check-expect (quick-sort-abstract (list 23) >) (list 23))
(check-expect (quick-sort-abstract (list 11 8 14 7) >) (list 14 11 8 7))
(check-expect (quick-sort-abstract (list 11 9 2 18 12 14 4 1) >)
              (list 18 14 12 11 9 4 2 1))
(check-expect
 (quick-sort-abstract (list 11 14 9 4 2 9 18 1 12 18 11 2 12 14 4 1) >)
 (list 18 18 14 14 12 12 11 11 9 9 4 4 2 2 1 1))
(check-expect
 (quick-sort-abstract (list 11 14 9 4 2 9 18 1 12 18 11 2 12 14 4 1) >=)
 (list 18 18 14 14 12 12 11 11 9 9 4 4 2 2 1 1))