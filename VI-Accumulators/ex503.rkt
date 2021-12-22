;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex503) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Matrix is one of: 
;  – (cons Row '())
;  – (cons Row Matrix)
; Constraint: all rows in Matrix are of the same length.
 
; A Row is one of: 
;  – '() 
;  – (cons Number Row)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define ALL-ZERO-ERROR "all rows start with a zero")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Matrix -> Matrix 
; finds a row that doesn't start with 0 and
; uses it as the first one
; generative moves the first row to last place 
; no termination if all rows start with 0
(define (rotate M)
  (local (; Matrix -> Matrix
          ; Does the actual work.
          (define (main M0)
            (cond
              [(not (= (first (first M)) 0)) M]
              [else
               (rotate (append (rest M) (list (first M))))])))
    ; – IN –
    (cond
      [(all-leading-coefficients-zero? M)
       (error ALL-ZERO-ERROR)]
      [else (main M)])))

(check-error (rotate (list (list 0 1 2)
                           (list 0 3 4)
                           (list 0 5 6)))
             ALL-ZERO-ERROR)
(check-expect (rotate '((0 4 5) (1 2 3)))
              '((1 2 3) (0 4 5)))

; Matrix -> Matrix 
; Find a row that doesn't start with 0 and
; use it as the first one.
(define (rotate.v2 M0)
  (local (; Matrix [List-of Row] -> Matrix 
          ; The accumulator collects all rows
          ; from M that start with a 0 in
          ; reversed order.
          (define (rotate/a M seen)
            (cond
;              [(empty? (rest M)) ...] ; Can this be simplified to (empty? M)
              ; NOTE ---------------------------------------------------------
              ; Yes, it can be simplified.
              ; 1) Matrix is defined to be non-empty and as long as the
              ;    signature will be honored, this condition will only be true
              ;    when all rows start with a 0.
              ; 2) If all rows start with a zero, an error will be raised by
              ;    all-leading-coefficients-zero?
              ; 3) Would there be no error raised by
              ;    all-leading-coefficients-zero?, this function would return
              ;    M0 in reversed order.
              ; END NOTE -----------------------------------------------------
              [(empty? M) seen]
              [else
               (if (= (first (first M)) 0)
                   (rotate/a (rest M) (cons (first M) seen))
                   (append M seen))])))
    ; – IN –
    (cond
      [(all-leading-coefficients-zero? M0)
       (error ALL-ZERO-ERROR)]
      [else
       (rotate/a M0 '())])))

(check-error (rotate.v2 (list (list 0 1 2)
                              (list 0 3 4)
                              (list 0 5 6)))
             ALL-ZERO-ERROR)
(check-expect (rotate.v2 '((0 4 5) (1 2 3)))
              '((1 2 3) (0 4 5)))

; from ex468.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Matrix -> Boolean
; Are all leading coefficients
; in m zero?
(define (all-leading-coefficients-zero? m)
  (cond
    [(empty? m) #true]
    [else
     (and (zero? (first (first m)))
          (all-leading-coefficients-zero? (rest m)))]))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; How many lists do we get if M consists of 5,000 lines?

    ; Rotate would have to iterate over the 5000 lines until it reaches the last
    ; one, which does not start with a 0. Each time append would create a new
    ; list with 5000 lists as its items, therefore one would get
    ;     5000 * 5000 = 25 000 000
    ; lists, that is 25 million.

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define size 10)
(define row-size 10)
(define zero-list (build-list row-size (lambda (x) 0)))
(define non-zero-list (build-list row-size (lambda (x) 1)))
(define matrix (append (build-list (sub1 size) (lambda (x) zero-list))
                       (list non-zero-list)))

(time (rotate.v2 matrix))


; rows in matrix  |  1000  |  2000  |  3000  |  4000  |   5000
; rotate          |   422  |  1879  |  4310  |  6916  |  11212
; rotate.v2       |     4  |     9  |     7  |     8  |     22
