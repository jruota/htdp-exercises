;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex446) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
