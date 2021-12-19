;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex502) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [NEList-of 1String] -> [NEList-of 1String]
; Create a palindrome from s0.
(define (mirror s0)
  (append (all-but-last s0)
          (list (last s0))
          (reverse (all-but-last s0))))

(check-expect (mirror (explode "abc")) (explode "abcba"))

; [NEList-of 1String] -> [NEList-of 1String]
; Create a palindrome from s0.
(define (mirror.v2 nelo1s)
  (local (; [NEList-of 1String] -> [NEList-of 1String]
          ; Create a palindrome from nelo1s0.
          ; The accumulator accu collects all 1Strings
          ; from nelo1s0 up to the last one (exclusive)
          ; in reverse order.
          (define (mirror/a nelo1s0 accu)
            (cond
              [(empty? (rest nelo1s0))
               (append nelo1s accu)]
              [else
               (mirror/a (rest nelo1s0)
                         (cons (first nelo1s0) accu))])))
    ; – IN –
    (mirror/a nelo1s '())))

(check-expect (mirror.v2 (explode "abc")) (explode "abcba"))

; [List-of X] -> [List-of X]
; Return all items from the list lox
; except the last one.
; If nelox is empty, return the empty list.
(define (all-but-last lox)
  (local (; [NEList-of X] -> [NEList-of X]
          ; Return all items from the non-empty
          ; list nelox except the last one.
          (define (ne-all-but-last nelox)
            (cond
              [(empty? (rest nelox)) '()]
              [else
               (cons (first nelox)
                     (all-but-last (rest nelox)))])))
    ; — IN –
    (cond
      [(empty? lox) '()]
      [else (ne-all-but-last lox)])))

(check-expect (all-but-last '()) '())
(check-expect (all-but-last (explode "abcdefg"))
              (explode "abcdef"))

; [List-of X] -> [List-of X]
; Return the last item in lox.
; If lox is empty, return the empty list.
(define (last lox)
  (local (; [NEList-of X] -> X
          ; Return the last item in nelox.
          (define (ne-last nelox)
            (cond
              [(empty? (rest nelox)) (first nelox)]
              [else
               (ne-last (rest nelox))])))
    ; – IN –
    (cond
      [(empty? lox) '()]
      [else
       (ne-last lox)])))

(check-expect (last '()) '())
(check-expect (last (list 1 2 3 4 5)) 5)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define size 50000)
(define word "abcdefg")
(define palindrome-list (build-list size (lambda (x) (explode word))))

(time (foldl (lambda (x y) (mirror x)) '() palindrome-list))
(time (foldr (lambda (x y) (mirror x)) '() palindrome-list))

(time (foldl (lambda (x y) (mirror.v2 x)) '() palindrome-list))
(time (foldr (lambda (x y) (mirror.v2 x)) '() palindrome-list))
