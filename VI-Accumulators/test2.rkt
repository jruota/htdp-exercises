;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname test2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of [List-of String]] -> [List-of [List-of String]]
; Format in such that the overall organization
; of the text is preserved, that is it contains
; all whitespace and all newlines.
(define (test in)
  (local (; [List-of [List-of String]]
          ; [List-of [List-of String]]
          ; [List-of [List-of String]]
          ; ->
          ; [List-of [List-of String]]
          ; Format input such that the overall organization
          ; of the text is preserved, that is it contains
          ; all whitespace and all newlines.
          ; The accumulator para collects the current paragraph,
          ; the accumulator result collects the so far formatted
          ; paragraphs, such that input = result + para.
          ; The preceding "formula" is just a hint and should
          ; not be taken literally.
          (define (test/a input para result)
            (cond
              [(and (empty? input) (empty? para) (empty? result))
               result]
              [(and (empty? input) (empty? para) (cons? result))
               result]
              [(and (empty? input) (cons? para) (empty? result))
               (list para)]
              [(and (empty? input) (cons? para) (cons? result))
               (append result (list para))]
              [(and (cons? input) (empty? para) (empty? result))
               (if (empty? (first input))
                   (test/a (rest input) '() (list '()))
                   (test/a (rest input) (first input) '()))]
              [(and (cons? input) (empty? para) (cons? result))
               (if (empty? (first input))
                   (test/a (rest input) '() (append result (list '())))
                   (test/a (rest input) (first input) result))]
              [(and (cons? input) (cons? para) (empty? result))
               (if (empty? (first input))
                   (test/a (rest input) '() (append (list para) (list '())))
                   (test/a (rest input) (append para (first input)) '()))]
              [(and (cons? input) (cons? para) (cons? result))
               (if (empty? (first input))
                   (test/a (rest input)
                           '()
                           (append result (list para) (list '())))
                   (test/a (rest input) (append para (first input)) result))])))
    ; – IN –
    (test/a in '() '())))

(check-expect (test '()) '())

(check-expect (test (list '()))
              (list '()))
(check-expect (test (list '() '()))
              (list '() '()))
(check-expect (test (list '() '() '()))
              (list '() '() '()))
(check-expect (test (list '() '() '() '()))
              (list '() '() '() '()))

(check-expect (test (list (list "ab" "cd" "ef")))
              (list (list "ab" "cd" "ef")))
(check-expect (test (list (list "ab" "cd" "ef")
                             (list "gh" "ij" "kl")))
              (list (list "ab" "cd" "ef" "gh" "ij" "kl")))

(check-expect (test (list '()
                          (list "ab" "cd" "ef")
                          (list "gh" "ij" "kl")))
              (list '()
                    (list "ab" "cd" "ef" "gh" "ij" "kl")))
(check-expect (test (list (list "ab" "cd" "ef")
                          '()
                          (list "gh" "ij" "kl")))
              (list (list "ab" "cd" "ef")
                    '()
                    (list "gh" "ij" "kl")))
(check-expect (test (list (list "ab" "cd" "ef")
                          (list "gh" "ij" "kl")
                          '()))
              (list (list "ab" "cd" "ef" "gh" "ij" "kl")
                    '()))

(check-expect (test (list (list "ab" "cd" "ef")
                          (list "gh" "ij" "kl")
                          '()
                          (list "mn" "op" "qr")
                          (list "st" "uv" "wx")
                          '()))
              (list (list "ab" "cd" "ef" "gh" "ij" "kl")
                    '()
                    (list "mn" "op" "qr" "st" "uv" "wx")
                    '()))

(check-expect (test (list '()
                          (list "ab" "cd" "ef")
                          (list "gh" "IJ" "kl")
                          '()
                          (list "mn" "op")
                          (list "qr" "st" "uv" "wx")
                          '()
                          '()
                          (list "yZ")
                          '()))
              (list '()
                    (list "ab" "cd" "ef" "gh" "IJ" "kl")
                    '()
                    (list "mn" "op" "qr" "st" "uv" "wx")
                    '()
                    '()
                    (list "yZ")
                    '()))

; [List-of [List-of String]] -> [List-of [List-of String]]
; Format in such that the overall organization
; of the text is preserved, that is it contains
; all whitespace and all newlines.
(define (test.v2 in)
  (local (; [List-of [List-of String]]
          ; [List-of [List-of String]]
          ; [List-of [List-of String]]
          ; ->
          ; [List-of [List-of String]]
          ; Format input such that the overall organization
          ; of the text is preserved, that is it contains
          ; all whitespace and all newlines.
          ; The accumulator para collects the current paragraph,
          ; the accumulator result collects the so far formatted
          ; paragraphs, such that input = result + para.
          ; The preceding "formula" is just a hint and should
          ; not be taken literally.
          (define (test/a input para result)
            (cond
              [(and (empty? input) (empty? para))
               result]
              [(and (empty? input) (cons? para))
               (append result (list para))]
              [(and (cons? input) (empty? para))
               (if (empty? (first input))
                   (test/a (rest input) '() (append result (list '())))
                   (test/a (rest input) (first input) result))]
              [(and (cons? input) (cons? para))
               (if (empty? (first input))
                   (test/a (rest input)
                           '()
                           (append result (list para) (list '())))
                   (test/a (rest input) (append para (first input)) result))])))
    ; – IN –
    (test/a in '() '())))

(check-expect (test.v2 '()) '())

(check-expect (test.v2 (list '()))
              (list '()))
(check-expect (test.v2 (list '() '()))
              (list '() '()))
(check-expect (test.v2 (list '() '() '()))
              (list '() '() '()))
(check-expect (test.v2 (list '() '() '() '()))
              (list '() '() '() '()))

(check-expect (test.v2 (list (list "ab" "cd" "ef")))
              (list (list "ab" "cd" "ef")))
(check-expect (test.v2 (list (list "ab" "cd" "ef")
                             (list "gh" "ij" "kl")))
              (list (list "ab" "cd" "ef" "gh" "ij" "kl")))

(check-expect (test.v2 (list '()
                             (list "ab" "cd" "ef")
                             (list "gh" "ij" "kl")))
              (list '()
                    (list "ab" "cd" "ef" "gh" "ij" "kl")))
(check-expect (test.v2 (list (list "ab" "cd" "ef")
                             '()
                             (list "gh" "ij" "kl")))
              (list (list "ab" "cd" "ef")
                    '()
                    (list "gh" "ij" "kl")))
(check-expect (test.v2 (list (list "ab" "cd" "ef")
                             (list "gh" "ij" "kl")
                             '()))
              (list (list "ab" "cd" "ef" "gh" "ij" "kl")
                    '()))

(check-expect (test.v2 (list (list "ab" "cd" "ef")
                             (list "gh" "ij" "kl")
                             '()
                             (list "mn" "op" "qr")
                             (list "st" "uv" "wx")
                             '()))
              (list (list "ab" "cd" "ef" "gh" "ij" "kl")
                    '()
                    (list "mn" "op" "qr" "st" "uv" "wx")
                    '()))

(check-expect (test.v2 (list '()
                             (list "ab" "cd" "ef")
                             (list "gh" "IJ" "kl")
                             '()
                             (list "mn" "op")
                             (list "qr" "st" "uv" "wx")
                             '()
                             '()
                             (list "yZ")
                             '()))
              (list '()
                    (list "ab" "cd" "ef" "gh" "IJ" "kl")
                    '()
                    (list "mn" "op" "qr" "st" "uv" "wx")
                    '()
                    '()
                    (list "yZ")
                    '()))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(require 2htdp/batch-io)

(define input (read-words/line "sample-input.txt"))
(define paragraphs-formatted (test.v2 input))
paragraphs-formatted
(length paragraphs-formatted)
