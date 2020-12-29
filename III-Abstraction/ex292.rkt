;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex292) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X] [X X -> Boolean] [NEList-of X] -> Boolean 
; Is l sorted according to cmp?
(define (sorted? cmp l)
  (cond
    [(empty? (rest l)) #true]
    [else
     (and (cmp (first l) (second l))
          (sorted? cmp (rest l)))]))

(check-expect (sorted? < '(1 2 3)) #true)
(check-expect (sorted? < '(2 1 3)) #false)

; ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

; Could you redefine sorted to use sorted? from exercise 292?

; [X X -> Boolean] -> [[List-of X] -> Boolean]
; is the given list l0 sorted according to cmp
(define (sorted cmp)
  (lambda (l0)
    (local (; [NEList-of X] -> Boolean 
            ; is l sorted according to cmp
            (define (sorted/l l)
              (cond
                [(empty? (rest l)) #true]
                [else (and (cmp (first l) (second l))
                           (sorted/l (rest l)))])))
      (if (empty? l0) #true (sorted/l l0)))))

(check-expect [(sorted <) '()] #true)
(check-expect [(sorted string<?) '("b" "c")] #true)
(check-expect [(sorted <) '(1 2 3 4 5 6)] #true)

; NOTE +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; The following works, because (so it seems) the inner "lambda" definition has
; access to the parameter "cmp" of the surrounding "sorted.v2".
; Anyway, "cmp" must be passed to "sorted?" if one wants to avoid reformulating
; or redefining "sorted?".

; END NOTE +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; [X X -> Boolean] -> [[List-of X] -> Boolean]
; is the given list l0 sorted according to cmp
(define (sorted.v2 cmp)
  (lambda (l0)
    (if (empty? l0) #true (sorted? cmp l0))))

(check-expect [(sorted.v2 <) '()] #true)
(check-expect [(sorted.v2 string<?) '("b" "c")] #true)
(check-expect [(sorted.v2 <) '(1 2 3 4 5 6)] #true)

; ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

; Explain why sorted/l does not consume cmp as an argument.

; "sorted/l" is defined locally and thus has access to all paremeters/arguments
; of any enclosing functions.