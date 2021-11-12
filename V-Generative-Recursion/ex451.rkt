;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex451) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct table [length array])
; A Table is a structure:
;   (make-table N [N -> Number])

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Table -> N
; Find the root index of t.
; A root index is a natural number i such that (table-ref t i)
; is a root of table t.
; The root of a table t is a number in (table-array t) that is close to 0.
; RESTRICTION: the table t has to be monotonically increasing.
; RESTRICTION: the table has to have a root.
(define (find-linear t)
  (local (; N -> N
          ; Find the root index of t.
          (define (helper-linear n current)
            (cond
              [(>= n (table-length t))
               current]
              [else
               (if (<= (abs (table-ref t n)) (abs (table-ref t current)))
                   (helper-linear (add1 n) n)
                   (helper-linear (add1 n) current))])))
    ; – IN –
    (helper-linear 0 0)))

(check-expect (find-linear (make-table 1024 linear))
              9)
(check-expect (find-linear (make-table 1024 quadratic))
              1)
(check-expect (find-linear (make-table 1024 linear1023))
              1023)
(check-expect (find-linear (make-table 1024 linear0))
              0)

; Table -> N
; Find the root index of t.
; A root index is a natural number i such that (table-ref t i)
; is a root of table t.
; The root of a table t is a number in (table-array t) that is close to 0.
; RESTRICTION: the table t has to be monotonically increasing.
; RESTRICTION: the table has to have a root.
; TERMINATION: uses generative recursion, halving the possible solutions
;              with every step.
(define (find-binary t)
  (local ((define left 0)
          (define right (- (table-length t) 1))
          (define table@left (table-ref t left))
          (define table@right (table-ref t right))

          ; N N Number Number -> N
          ; Find the root index of t.
          (define (helper-binary l r t@l t@r)
            (cond
              [(<= (- r l) 1)
               (if (<= (abs t@l) (abs t@r))
                   l
                   r)]
              [else
               (local ((define mid (ceiling (/ (+ l r) 2)))
                       (define table@mid (table-ref t mid)))
                 ; – IN –
                 (cond
                   [(<= t@l 0 table@mid)
                    (helper-binary l mid t@l table@mid)]
                   [(<= table@mid 0 t@r)
                    (helper-binary mid r table@mid t@r)]))])))
    ; – IN –
    (helper-binary left right table@left table@right)))
    

(check-expect (find-binary (make-table 1024 linear))
              9)
(check-expect (find-binary (make-table 1024 quadratic))
              1)
(check-expect (find-binary (make-table 1024 linear1023))
              1023)
(check-expect (find-binary (make-table 1024 linear0))
              0)

; Table N -> Number
; looks up the ith value in array of t
(define (table-ref t i)
  ((table-array t) i))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (linear x)
  (+ (* 2 x) -18))

(define (linear1023 x)
  (- (* 4 x) 4092))

(define (linear0 x)
  (* 2 x))

(define (quadratic x)
  (- (* 1/2 (expt (+ x 3) 2)) 8))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; How many calls to find are needed in find-linear and find-binary,
; respectively?

; For find-linear, if the root is at the nth position, (+ n 1) calls
; are needed (+1 because the first call is at position 0).

; For find-binary, if the root is at the nth position, (/ (log S) (log 0.5))
; calls are needed, where S is the interval to be searched for the root.

; DERIVING THE FORMULA ---------------------------------------------------------

; If one is to find a root in the interval [a, b] and S = (- b a), a solution
; is found if S <= 1.

; The interval S is halved until the condition is met,
; i.e. (* S (expt 0.5 n)) <= 1.

; Solve for n:

;    (* S (expt 0.5 n)) <= 1
;          (expt 0.5 n) <= (/ 1 S)
;    (log (expt 0.5 n)) <= (log (/ 1 S))
;       (* n (log 0.5)) <= (log (/ 1 S))         [exponent logarithm rule]
;       (* n (log 0.5)) <= (- (log 1) (log S))   [division logarithm rule]
;                     n <= (/ (- (log 1) (log S)) (log 0.5))

; END OF DERIVATION ------------------------------------------------------------
