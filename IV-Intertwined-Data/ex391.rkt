;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex391) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of Number] [List-of Number] -> [List-of Number]
; Replace the final '() in front with end.
(define (replace-eol-with front end)
  (cond
    [(and (empty? front) (empty? end)) '()]
    [(and (empty? front) (cons? end)) end]
    [(and (cons? front) (empty? end)) front]
    [(and (cons? front) (cons? end))
     (cons (first front)
           (replace-eol-with (rest front) end))]))

(check-expect (replace-eol-with '() '())
              '())
(check-expect (replace-eol-with '() (list 10 20))
              (list 10 20))
(check-expect (replace-eol-with (cons 2 (cons 1 '())) '())
              (cons 2 (cons 1 '())))
(check-expect (replace-eol-with (cons 2 (cons 1 '())) (list 10))
              (cons 2 (cons 1 (cons 10 '()))))

; [List-of Number] [List-of Number] -> [List-of Number]
; Replace the final '() in front with end.
(define (replace-eol-with.v2 front end)
  (cond
    [(empty? front) end]
    [(cons? front)
     (cons (first front)
           (replace-eol-with.v2 (rest front) end))]))

(check-expect (replace-eol-with.v2 '() '())
              '())
(check-expect (replace-eol-with.v2 '() (list 10 20))
              (list 10 20))
(check-expect (replace-eol-with.v2 (cons 2 (cons 1 '())) '())
              (cons 2 (cons 1 '())))
(check-expect (replace-eol-with.v2 (cons 2 (cons 1 '())) (list 10))
              (cons 2 (cons 1 (cons 10 '()))))
