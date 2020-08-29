;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex213) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)

; A List-of-words is one of:
; – '()
; – (cons Word List-of-words)
; Interpretation:
;     A list containing words.

; DATA EXAMPLES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define WORD1 empty)
(define WORD2 (list "a" "p" "p" "l" "e"))
(define WORD3 (list "b" "a" "n" "a" "n" "a"))
(define WORD4 (list "d" "e"))

(define LOW1 (list WORD1 WORD2 WORD3))
(define LOW2 (list (list "d" "e")
                   (list "e" "d")))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Word -> List-of-words
; creates all rearrangements of the letters in w
(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else (insert-everywhere/in-all-words (first w)
            (arrangements (rest w)))]))

(check-expect (arrangements '())
              (list '()))
(check-member-of (arrangements WORD4)
                 LOW2
                 (reverse LOW2))
(check-expect (arrangements (explode "rat"))
              (list (list "r" "a" "t")
                    (list "a" "r" "t")
                    (list "a" "t" "r")
                    (list "r" "t" "a")
                    (list "t" "r" "a")
                    (list "t" "a" "r")))

; 1String List-of-words -> List-of-words
; Return low with l inserted at the beginning,
; between all letters, and at the end of all words
; of low.
(define (insert-everywhere/in-all-words l low)
  (cond
    [(empty? low) '()]
    [(cons? low)
     (append (insert-everywhere/in-word l (first low))
             (insert-everywhere/in-all-words l (rest low)))]))

(check-expect (insert-everywhere/in-all-words "a" '())
              '())
(check-expect (insert-everywhere/in-all-words "a" (list (list "b")))
              (list (list "a" "b")
                    (list "b" "a")))
(check-expect (insert-everywhere/in-all-words "d"
                                              (list (list "e" "r")
                                                    (list "r" "e")))
              (list (list "d" "e" "r")
                    (list "e" "d" "r")
                    (list "e" "r" "d")
                    (list "d" "r" "e")
                    (list "r" "d" "e")
                    (list "r" "e" "d")))

; 1String Word -> List-of-words
; Return a list of words with l inserted
; at the beginning, between all letters, and
; at the end of w.
(define (insert-everywhere/in-word l w)
  (cond
    [(empty? w) (list (list l))]
    [(cons? w)
     (append (list (cons l w))
             (insert-at-the-beginning/in-word
              (first w)
              (insert-everywhere/in-word
               l
               (rest w))))]))

(check-expect (insert-everywhere/in-word "a" '())
              (list (list "a")))
(check-expect (insert-everywhere/in-word "a" (list "b"))
              (list (list "a" "b")
                    (list "b" "a")))
(check-expect (insert-everywhere/in-word "a" (list "b" "c"))
              (list (list "a" "b" "c")
                    (list "b" "a" "c")
                    (list "b" "c" "a")))

; 1String List-of-words -> List-of-words
; Return a list of words with l inserted
; at the beginning of every element in low.
(define (insert-at-the-beginning/in-word l low)
  (cond
    [(empty? low) '()]
    [(cons? low)
     (cons (cons l (first low))
           (insert-at-the-beginning/in-word l (rest low)))]))

(check-expect (insert-at-the-beginning/in-word "a" '())
              '())
(check-expect (insert-at-the-beginning/in-word "a" (list (list "b")))
              (list (list "a" "b")))
(check-expect (insert-at-the-beginning/in-word "a" (list (list "b")
                                                         (list "c")))
              (list (list "a" "b")
                    (list "a" "c")))