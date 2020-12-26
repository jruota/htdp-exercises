;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex274) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A [List-of ITEM] is one of:
;     – '()
;     – (cons ITEM [List-of ITEM])
; Interpretation:
;     A list of ITEMs.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of 1String] -> [List-of [List-of 1String]]
; Produce the list of all prefixes. A list p
; is a prefix of l if p and l are the same up
; through all items in p.
(define (prefix-from-abstractions lo1s)
  (cond
    [(empty? lo1s) '()]
    [else
     (cons (foldr cons '() lo1s)
           (prefix-from-abstractions (remove-last lo1s)))]))

(check-expect (prefix-from-abstractions '())
              '())
(check-expect (prefix-from-abstractions (list "a"))
              (list (list "a")))
(check-expect (prefix-from-abstractions (list "a" "b" "c" "d"))
              (prefix (list "a" "b" "c" "d")))

; [List-of 1String] -> [List-of [List-of 1String]]
; Produce all suffixes. A list s is a suffix
; of l if p and l are the same from the end,
; up through all items in s.
(define (suffix-from-abstractions lo1s)
  (cond
    [(empty? lo1s) '()]
    [else
     (cons (foldr cons '() lo1s)
           (suffix-from-abstractions (rest lo1s)))]))

(check-expect (suffix-from-abstractions '())
              '())
(check-expect (suffix-from-abstractions (list "a"))
              (list (list "a")))
(check-expect (suffix-from-abstractions (list "a" "b" "c" "d"))
              (suffix (list "a" "b" "c" "d")))

; from ex190.rkt +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; [List-of 1String] -> [List-of [List-of 1String]]
; Produce the list of all prefixes. A list p
; is a prefix of l if p and l are the same up
; through all items in p.
(define (prefix lo1s)
  (cond
    [(empty? lo1s) '()]
    [(cons? lo1s)
     (cons lo1s
           (prefix (remove-last lo1s)))]))

(check-expect (prefix '())
              '())
(check-expect (prefix (list "a"))
              (list (list "a")))
(check-expect (prefix (list "a" "b" "c" "d"))
              (list (list "a" "b" "c" "d")
                    (list "a" "b" "c")
                    (list "a" "b")
                    (list "a")))

; [List-of 1String] -> [List-of [List-of 1String]]
; Produce all suffixes. A list s is a suffix
; of l if p and l are the same from the end,
; up through all items in s.
(define (suffix lo1s)
  (cond
    [(empty? lo1s) '()]
    [(cons? lo1s)
     (cons lo1s
           (suffix (rest lo1s)))]))

(check-expect (suffix '())
              '())
(check-expect (suffix (list "a"))
              (list (list "a")))
(check-expect (suffix (list "a" "b" "c" "d"))
              (list (list "a" "b" "c" "d")
                    (list "b" "c" "d")
                    (list "c" "d")
                    (list "d")))

; [List-of 1String] -> [List-of 1String]
; Remove the last element of loi.
(define (remove-last loi)
  (cond
    [(empty? loi) '()]
    [(cons? loi)
     (if (empty? (rest loi))
         '()
         (cons (first loi) (remove-last (rest loi))))]))

(check-expect (remove-last '())
              '())
(check-expect (remove-last (list "a"))
              '())
(check-expect (remove-last (list "a" "b" "c"))
              (list "a" "b"))

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++