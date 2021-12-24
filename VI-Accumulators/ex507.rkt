;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex507) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X Y] N [X -> Y] -> [List-of Y]
; Create a list of n elements by
; applying proc to the integers
; from 0 to (sub1 n) in order.
(define (build-l*st n proc)
  (local (; N -> [List-of Y]
          ; Create a list of n elements by
          ; applying proc to the integers
          ; from n0 to 0 in order.
          (define (helper-build-l*st n0)
            (cond
              [(zero? n0)
               (cons (proc n0) '())]
              [else
               (cons (proc n0)
                     (helper-build-l*st (sub1 n0)))])))
    ; – IN –
    (if (zero? n)
        '()
        (reverse (helper-build-l*st (sub1 n))))))

(check-expect (build-l*st 0 (lambda (x) x))
              '())
                        
(define n 10)
(define f (lambda (x) "hello"))
;(define f (lambda (x) (add1 x)))
(check-expect (build-l*st n f) (build-list n f))

; [X Y] N [X -> Y] -> [List-of Y]
; Create a list of n elements by
; applying proc to the integers
; from 0 to (sub1 n) in order.
(define (build-l*st.v2 n proc)
  (local (; N -> [List-of Y]
          ; Create a list of n elements by
          ; applying proc to the integers
          ; from n0 to 0 in order.
          ; The accumulator accu collects the
          ; the result of the first x applications
          ; of proc on n0 in a list in reverse order
          ; where x + n0 = n.
          (define (build-l*st/a n0 accu)
            (cond
              [(zero? n0)
               (cons (proc n0) accu)]
              [else
               (build-l*st/a (sub1 n0)
                             (cons (proc n0) accu))])))
    ; – IN –
    (if (zero? n)
        '()
        (build-l*st/a (sub1 n) '()))))

(check-expect (build-l*st.v2 0 (lambda (x) x))
              '())
                        
(define n1 10)
;(define f1 (lambda (x) "hello"))
(define f1 (lambda (x) (add1 x)))
(check-expect (build-l*st.v2 n1 f1) (build-list n1 f1))

; [X Y] [X Y -> Y] Y [List-of X] -> Y
; Apply f to all items in l0.
; Combine the return values as defined by f.
; For the initial combination step, use e as
; the initial value.
(define (f*ldl f e l0)  
  (local (; [List-of Y] [List-of X] -> [List-of Y]
          ; Apply f to all items in l.
          ; Combine the return values as defined by f.
          ; For the initial combination step, use e as
          ; the initial value.
          ; The accumulator a collects an intermiadary
          ; result of the first n items that are on l0
          ; but not on l.
          (define (fold/a a l)
            (cond
              [(empty? l) a]
              [else
               (fold/a (f (first l) a) (rest l))])))
    ; – IN –
    (fold/a e l0)))

(check-expect (f*ldl + 0 '())
              0)
(check-expect (f*ldl cons '() '(1 2 3 4))
              '(4 3 2 1))
(check-expect (f*ldl + 0 '(1 2 3 4))
              10)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define size 1000000)
(define process (lambda (x) x))
(define process2 (lambda (x y) x))

(time (foldr process2 0 (build-list size process)))
(time (foldr process2 0 (build-l*st size process)))
(time (foldr process2 0 (build-l*st.v2 size process)))
