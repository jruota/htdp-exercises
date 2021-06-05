;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex393) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A SON (set of numbers) is one of: 
; – empty 
; – (cons Number SON)
; Constraint:
;     No number occurs twice.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; SON SON -> SON
; Return a set containing elements from
; son1 and son2 without duplicates.
(define (union son1 son2)
  (local (; [List-of X] Y -> [List-of Z]
          ; Insert y at the end of lox.
          (define (insert-at-end lox y)
            (cond
              [(empty? lox) (cons y '())]
              [else
               (cons (first lox)
                     (insert-at-end (rest lox) y))])))
    ; – IN –
    (cond
      [(empty? son1) son2]
      [(empty? son2) son1]
      [(cons? son2)
       (if (member? (first son2) son1)
           (union son1 (rest son2))
           (union (insert-at-end son1 (first son2))
                  (rest son2)))])))

(check-expect (union '() '())
              '())
(check-expect (union '() (list 4 5 6 7 8 9 10))
              (list 4 5 6 7 8 9 10))
(check-expect (union (list 1 2 3 4 5 6 7) '())
              (list 1 2 3 4 5 6 7))
(check-expect (union (list 1 2 3 4 5 6 7) (list 4 5 6 7 8 9 10))
              (list 1 2 3 4 5 6 7 8 9 10))

; SON SON -> SON
; Produce the set of exactly those elements
; that occur in both son1 and son2.
(define (intersect son1 son2)
  (cond
    [(empty? son1) '()]
    [(cons? son1)
     (if (member? (first son1) son2)
         (cons (first son1) (intersect (rest son1) son2))
         (intersect (rest son1) son2))]))

(check-expect (intersect '() '())
              '())
(check-expect (intersect '() (list 2 4 6 8 10))
              '())
(check-expect (intersect (list 1 3 5 7 9 11) '())
              '())
(check-expect (intersect (list 1 3 5 7 9 11) (list 2 4 6 8 10))
              '())
(check-expect (intersect (list 1 2 3 5 7 9 10 11) (list 2 4 6 8 9 10))
              (list 2 9 10))