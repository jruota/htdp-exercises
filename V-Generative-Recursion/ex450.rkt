;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex450) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define ε 0.000000001)
(define DELTA 0.001)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R,(+ R ε)]
; assume f is continuous 
; assume (<= (f left) 0 (f right)) for (< left right),
;        i.e. the function is monotonically increasing
; generative divides interval in half, the root is in one of the two
; halves, picks according to assumption 
(define (find-root f left right)
  (cond
    [(<= (- right left) ε) left]
    [else
     (local ((define f@left (f left))
             (define f@right (f right))

             ; Number Number Number Number -> Number
             ; Helper function to find-root
             ; that avoids recomputations.
             (define (root-helper l r f@l f@r)
               (cond
                 [(<= (- r l) ε) l]
                 [else
                  (local ((define mid (/ (+ l r) 2))
                          (define f@m (f mid)))
                    ; – IN –
                    (cond
                      [(<= f@l 0 f@m)    ; changed
                       (root-helper l mid f@l f@m)]
                      [(<= f@m 0 f@r)    ; changed
                       (root-helper mid r f@m f@r)]))])))
       ; – IN –
       (root-helper left right f@left f@right))]))

(check-satisfied (find-root poly 3.1 19) (range-checker 3.1 19))
(check-satisfied (find-root poly 3.1 4.0000000001)
                 (range-checker 3.1 4.0000000001))

(check-expect (find-root poly 3.9999999999 4.0000000001)
              3.9999999999)

(check-satisfied (find-root poly 3 6) (within-delta? poly))
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
