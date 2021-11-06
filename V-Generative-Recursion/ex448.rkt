;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex448) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define ε 0.000000001)
(define DELTA 0.001)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R,(+ R ε)]
; assume f is continuous 
; assume (or (<= (f left) 0 (f right)) (<= (f right) 0 (f left)))
; generative divides interval in half, the root is in one of the two
; halves, picks according to assumption 
(define (find-root f left right)
  (cond
    [(<= (- right left) ε) left]
    [else
      (local ((define mid (/ (+ left right) 2))
              (define f@mid (f mid)))
        (cond
          [(or (<= (f left) 0 f@mid) (<= f@mid 0 (f left)))
           (find-root f left mid)]
          [(or (<= f@mid 0 (f right)) (<= (f right) 0 f@mid))
           (find-root f mid right)]))]))

(check-satisfied (find-root poly 1 3) (range-checker 1 3))
(check-satisfied (find-root poly 3 19) (range-checker 3 19))

(check-satisfied (find-root poly 1 3) (within-delta? poly))
(check-satisfied (find-root poly 3 19) (within-delta? poly))

; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

; Number Number -> [Number -> Boolean]
; Create a predicate that checks whether a number
; is within the range [a, b], where a <= b.
(define (range-checker a b)
  (local (; Is x within the range [a, b]?
          ; Number -> Boolean
          (define (within-range? x)
            (and (<= a x) (<= x b))))
    ; – IN –
    within-range?))

; [Number -> Number] -> [Number -> Boolean]
; Create a predicate that checks whether the result
; of (f a) is within DELTA of 0.
(define (within-delta? f)
  (local (; Number -> Boolean
          ; Is a within DELTA of 0?
          (define (main a)
            (<= (f a) DELTA)))
    ; – IN –
    main))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; The find-root algorithm terminates for all (continuous) f, left, and right
; for which the assumption holds. Why? Formulate a termination argument.

; Hint Suppose the arguments of find-root describe an interval of size S1.
; How large is the distance between left and right for the first and second
; recursive call to find-root? After how many steps is (- right left) smaller
; than or equal to ε?

; The interval is halved for every recursive call of root. So first it is
; S1, then it is S1/2, then S1/4, S1/8 and so on. Ultimately it needs to
; satisfy the following inequality ε >= S1/n, and this gives
; n >= S1/ε. So the function will need S1/ε steps to find the root.
; This also shows that ε should be greater than zero, otherwise the root
; finding will loop forever.
