;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex190) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require racket/list)    ; for take function 

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A List-of-1Strings is one of:
; – '()
; – (cons 1String List-of-1Strings)
; Interpretation:
;     A list containing 1Strings.

; A List-of-List-of-1Strings is one of:
; – '()
; – (cons List-of-1Strings List-of-List-of-1Strings)
; Interpretation:
;     A list containing lists of 1Strings.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; List-of-1Strings -> List-of-List-of-1Strings
; Produce the list of all prefixes. A list p
; is a prefix of l if p and l are the same up
; through all items in p.
(define (prefix-alt lo1s)
  (cond
    [(empty? lo1s) '()]
    [(cons? lo1s)
     (cons lo1s
           (prefix (take lo1s (sub1 (length lo1s)))))]))

(check-expect (prefix-alt '())
              '())
(check-expect (prefix-alt (list "a" "b" "c" "d"))
              (list (list "a" "b" "c" "d")
                    (list "a" "b" "c")
                    (list "a" "b")
                    (list "a")))

; List-of-1Strings -> List-of-List-of-1Strings
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
(check-expect (prefix (list "a" "b" "c" "d"))
              (list (list "a" "b" "c" "d")
                    (list "a" "b" "c")
                    (list "a" "b")
                    (list "a")))

; List-of-1Strings -> List-of-List-of-1Strings
; Produce the list of all prefixes, sorted by length,
; shortest first, longest last.
; A list p is a prefix of l if p and l are the same up
; through all items in p.
(define (prefix-sorted lo1s)
  (sort-by-length (prefix lo1s)))

(check-expect (prefix-sorted '())
              '())
(check-expect (prefix-sorted (list "a" "b" "c" "d"))
              (list (list "a")
                    (list "a" "b")
                    (list "a" "b" "c")
                    (list "a" "b" "c" "d")))

; List-of-1Strings -> List-of-List-of-1Strings
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
(check-expect (suffix (list "a" "b" "c" "d"))
              (list (list "a" "b" "c" "d")
                    (list "b" "c" "d")
                    (list "c" "d")
                    (list "d")))

; List-of-1Strings -> List-of-List-of-1Strings
; Produce all suffixes, sorted by length, shortest
; first, longest last.
; A list s is a suffix  of l if p and l are the
; same from the end, up through all items in s.
(define (suffix-sorted lo1s)
  (sort-by-length (suffix lo1s)))

(check-expect (suffix-sorted '())
              '())
(check-expect (suffix-sorted (list "a" "b" "c" "d"))
              (list (list "d")
                    (list "c" "d")
                    (list "b" "c" "d")
                    (list "a" "b" "c" "d")))

; List-of-1Strings -> List-of-1Strings
; Remove the last element of lo1s.
(define (remove-last lo1s)
  (cond
    [(empty? lo1s) '()]
    [(cons? lo1s)
     (if (empty? (rest lo1s))
         '()
         (cons (first lo1s) (remove-last (rest lo1s))))]))

(check-expect (remove-last '())
              '())
(check-expect (remove-last (list "a" "b" "c"))
              (list "a" "b"))

; List-of-List-of-1Strings -> List-of-List-of-1Strings
; Sort the elements of lolo1s by length, shortest first,
; longest last.
(define (sort-by-length lolo1s)
  (cond
    [(empty? lolo1s) '()]
    [(cons? lolo1s)
     (insert (first lolo1s) (sort-by-length (rest lolo1s)))]))

(check-expect (sort-by-length '())
              '())
(check-expect (sort-by-length (list (list "a" "b" "c")
                                    (list "a" "b" "c" "d")
                                    (list "a" "b" "c")
                                    '()
                                    (list "a")
                                    (list "a" "b")))
              (list '()
                    (list "a")
                    (list "a" "b")
                    (list "a" "b" "c")
                    (list "a" "b" "c")
                    (list "a" "b" "c" "d")))

; List-of-1Strings List-of-List-of-1Strings -> List-of-List-of-1Strings
; Insert lo1s into the sorted (by length, ascending) lolo1s.
(define (insert lo1s lolo1s)
  (cond
    [(empty? lolo1s) (list lo1s)]
    [(cons? lolo1s)
     (if (<= (length lo1s) (length (first lolo1s)))
         (cons lo1s lolo1s)
         (cons (first lolo1s) (insert lo1s (rest lolo1s))))]))

(check-expect (insert (list "a" "b" "c") '())
              (list (list "a" "b" "c")))
(check-expect (insert (list "a" "b" "c")
                      (list (list "a")
                            (list "a" "b")
                            (list "a" "b" "c")
                            (list "a" "b" "c" "d")))
              (list (list "a")
                    (list "a" "b")
                    (list "a" "b" "c")
                    (list "a" "b" "c")
                    (list "a" "b" "c" "d")))