;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex419) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define JANUS
  (list 31.0
        #i2e+34
        #i-1.2345678901235e+80
        2749.0
        -2939234.0
        #i-2e+33
        #i3.2e+270
        17.0
        #i-2.4e+270
        #i4.2344294738446e+170
        1.0
        #i-8e+269
        0.0
        99.0))

(define JANUS-SORTED (sort JANUS <))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of Number] -> Number
; Calculate the sum of the numbers in lon.
(define (sum lon)
  (cond
    [(empty? lon) 0]
    [else (+ (first lon) (sum (rest lon)))]))

(check-expect (sum '()) 0)
(check-expect (sum (list 1)) 1)
(check-expect (sum (list 1 2 3 4 5)) 15)

; [List-of Number] -> [List-of Number]
; Calculate the cumulative sum of lon.
(define (cumulative-sum lon)
  (local ((define LENGTH (length lon))

          ; N -> [List-of Number]
          ; Collect the sums in a list.
          (define (main x)
            (cond
              [(> x LENGTH) '()]
              [else (cons (sum-n x lon) (main (add1 x)))]))

          ; N -> Number
          ; Add the first n elements of l.
          (define (sum-n n l)
            (cond
              [(zero? n) 0]
              [else (+ (first l) (sum-n (sub1 n) (rest l)))])))
    ; – IN –
    (cond
      [(empty? lon) '()]
      [else (main 1)])))

(check-expect (cumulative-sum '()) '())
(check-expect (cumulative-sum (list 1)) (list 1))
(check-expect (cumulative-sum (list 1 2 3 4 5)) (list 1 3 6 10 15))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(sum JANUS)              ; returns third element of the list
(sum (reverse JANUS))    ; returns first element of the reversed list (inexact)
(sum (sort JANUS <))     ; returns sixth element of the sorted list

; return the second largest number
(exact->inexact (sum (map inexact->exact JANUS)))
(exact->inexact (sum (map inexact->exact JANUS-SORTED)))

(cumulative-sum JANUS)
(+ #i3.2e+270 #i-2.4e+270)
(+ #i8e+269 #i-8e+269)
(+ 31.0        #i2e+34                #i-1.2345678901235e+80 2749.0
   -2939234.0  #i-2e+33               #i3.2e+270             17.0
   #i-2.4e+270 #i4.2344294738446e+170 1.0                    #i-8e+269)
(cumulative-sum JANUS-SORTED)
(+ #i-2.4e+270 #i-8e+269)
(+ #i-3.2e+270 #i3.2e+270)