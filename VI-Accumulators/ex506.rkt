;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex506) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X Y] [X -> Y] [List-of X] -> [List-of Y]
; Apply proc to the elements of lst
; from the first elements to the last.
(define (my-map proc lst)
  (cond
    [(empty? lst) '()]
    [else
     (cons (proc (first lst))
           (my-map proc (rest lst)))]))

(check-expect (my-map (lambda (x) x) '())
              '())
(check-expect (my-map (lambda (x) (+ x 1)) (list 1 2 3 4 5))
              (list 2 3 4 5 6))
(check-expect (my-map string-length (list "hello" "world" "banana" "kathmandu"))
              (list 5 5 6 9))

; [X Y] [X -> Y] [List-of X] -> [List-of Y]
; Apply proc to the elements of lst
; from the first elements to the last.
(define (my-map.v2 proc lst)
  (local (; [List-of X] [List-of Y] -> [List-of Y]
          ; Apply proc to all elements of lst0.
          ; The accumulator accu collects the results
          ; of the application of proc to the first n
          ; elements of the reversed lst that are not
          ; on lst0.
          (define (my-map/a lst0 accu)
            (cond
              [(empty? lst0) accu]
              [else
               (my-map/a (rest lst0)
                         (cons (proc (first lst0)) accu))])))
    ; – IN –
    (my-map/a (reverse lst) '())))

(check-expect (my-map.v2 (lambda (x) x) '())
              '())
(check-expect (my-map.v2 (lambda (x) (+ x 1)) (list 1 2 3 4 5))
              (list 2 3 4 5 6))
(check-expect (my-map.v2 string-length
                         (list "hello" "world" "banana" "kathmandu"))
              (list 5 5 6 9))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define size 900000)
(define random-list (build-list size (lambda (x) (random 9))))
(define process (lambda (x) (+ x 1)))
(define process2 (lambda (x y) (+ x y)))

(time (foldr process2 0 (map process random-list)))
(time (foldr process2 0 (my-map process random-list)))
(time (foldr process2 0 (my-map.v2 process random-list)))
