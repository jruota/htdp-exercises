;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex493) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of X] -> [List-of X]
; Construct the reverse of alox. 
(define (invert alox)
  (cond
    [(empty? alox) '()]
    [else
     (add-as-last (first alox) (invert (rest alox)))]))

(check-expect (invert '()) '())
(check-expect (invert '(a b c)) '(c b a))
 
; X [List-of X] -> [List-of X]
; Add an-x to the end of alox. 
(define (add-as-last an-x alox)
  (cond
    [(empty? alox) (list an-x)]
    [else
     (cons (first alox) (add-as-last an-x (rest alox)))]))

(check-expect (add-as-last 'a '()) '(a))
(check-expect (add-as-last 'a '(c b)) '(c b a))

; [List-of X] -> [List-of X]
; Construct the reverse of alox.
(define (my-invert alox)
  (local (; [List-of X] [List-of X] -> [List-of X]
          ; Construct the reverse of alox0.
          ; Collect intermediate solutions in accu.
          (define (my-invert-accu alox0 accu)
            (cond
              [(empty? alox0) accu]
              [else
               (my-invert-accu (rest alox0) (cons (first alox0) accu))])))
    ; – IN –
    (my-invert-accu alox '())))

(check-expect (my-invert '()) '())
(check-expect (my-invert '(a b c)) '(c b a))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; (invert '(a b c))
; == (add-as-last 'a (invert '(b c)))
; == (add-as-last 'a (add-as-last 'b (invert '(c))))
; == (add-as-last 'a (add-as-last 'b (add-as-last 'c (invert '()))))
; == (add-as-last 'a (add-as-last 'b (add-as-last 'c '())))
; == (add-as-last 'a (add-as-last 'b '(c)))
; == (add-as-last 'a '(c b))
; == '(c b a)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Argue that, in the terminology of Intermezzo 4: The Nature of Numbers,
; invert consumes O(n2) time when the given list consists of n items.

; The argumentation is the same as for relative->absolute from exercise 490.
; Here, if there was no auxiliary function, invert would use on the order of n
; steps where n is the number of items in the list. The function add-as-last
; uses on the order of n steps, where n is the number of items in the list.
; Every call to invert with a list of n items sets off n calls to add-as-last.
; To get the total amount of calls one can use the following formula:
;
;     (+ n (* 1/2 n (+ n 1))) = (* 1/2 (+ (* n n) (* 3 n)))
;
; So, invert uses on the order of (* n n) [n-squared] steps, where n
; is the number of items in a list.

; length of list    recursive calls to       calls to           
;     n             invert                   add-as-last        total calls
; ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
;     0             0                         0                  0
;     1             1                         1                  2
;     2             2                         3                  5
;     3             3                         6                  9
;     4             4                        10                 14
;     5             5                        15                 20
;    ...           ...                       ...                ...

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define size 500)

(time (invert (build-list size add1)))
(time (my-invert (build-list size add1)))
