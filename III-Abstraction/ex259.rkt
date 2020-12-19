;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex259) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; A List-of-strings is one of:
; – '()
; – (cons String List-of-strings)
; Interpretation:
;     A list containing strings.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Word -> List-of-words
; Create all rearrangements of the letters in w.
(define (arrangements.v2 w)
  (local (; 1String List-of-words -> List-of-words
          ; Return low with l inserted at the beginning,
          ; between all letters, and at the end of all words
          ; of low.
          (define (insert-everywhere/in-all-words l low)
            (cond
              [(empty? low) '()]
              [(cons? low)
               (append (insert-everywhere l (first low))
                       (insert-everywhere/in-all-words l (rest low)))]))

          ; 1String Word -> List-of-words
          ; Insert l at the beginning, between all letters,
          ; and at the end of w.
          (define (insert-everywhere l w)
            (cond
              [(empty? w) (list (list l))]
              [(cons? w)
               (cons
                (cons l w)
                (prepend (first w)
                         (insert-everywhere l (rest w))))]))

          ; 1String List-of-words -> List-of-words
          ; Prepend l to all words in low.
          (define (prepend l low)
            (cond
              [(empty? low) '()]
              [(cons? low)
               (cons (cons l (first low))
                     (prepend l (rest low)))])))
    
    ; – IN –
    (cond
      [(empty? w) (list '())]
      [else
       (insert-everywhere/in-all-words (first w)
                                       (arrangements.v2 (rest w)))])))

(check-expect (arrangements.v2 '())
              (list '()))

(check-expect (arrangements.v2 (list "d" "e" "r"))
              (list
               (list "d" "e" "r")
               (list "e" "d" "r")
               (list "e" "r" "d")
               (list "d" "r" "e")
               (list "r" "d" "e")
               (list "r" "e" "d")))

; NOTE -------------------------------------------------------------------------

; The following functions where redesigned as an exercise, so that they differ
; somewhat from the functions in exercise 213. Those differences are mostly
; the function names an therefore irrelevant.

; END NOTE ---------------------------------------------------------------------

;; Word -> List-of-words
;; Create all rearrangements of the letters in w.
;(define (arrangements w)
;  (cond
;    [(empty? w) (list '())]
;    [else
;     (insert-everywhere/in-all-words (first w)
;                                     (arrangements (rest w)))]))
;
;(check-expect (arrangements '())
;              (list '()))
;
;(check-expect (arrangements (list "d" "e" "r"))
;              (list
;               (list "d" "e" "r")
;               (list "e" "d" "r")
;               (list "e" "r" "d")
;               (list "d" "r" "e")
;               (list "r" "d" "e")
;               (list "r" "e" "d")))
;
;; 1String List-of-words -> List-of-words
;; Return low with l inserted at the beginning,
;; between all letters, and at the end of all words
;; of low.
;(define (insert-everywhere/in-all-words l low)
;  (cond
;    [(empty? low) '()]
;    [(cons? low)
;     (append (insert-everywhere l (first low))
;             (insert-everywhere/in-all-words l (rest low)))]))
;
;(check-expect (insert-everywhere/in-all-words "d" '())
;              '())
;
;(check-expect (insert-everywhere/in-all-words "d"
;                                              (list
;                                               (list "e")))
;              (list
;               (list "d" "e")
;               (list "e" "d")))
;
;(check-expect (insert-everywhere/in-all-words "d"
;                                              (cons (list "e" "r")
;                                                    (cons (list "r" "e")
;                                                          '())))
;              (list
;               (list "d" "e" "r")
;               (list "e" "d" "r")
;               (list "e" "r" "d")
;               (list "d" "r" "e")
;               (list "r" "d" "e")
;               (list "r" "e" "d")))
;
;; 1String Word -> List-of-words
;; Insert l at the beginning, between all letters,
;; and at the end of w.
;(define (insert-everywhere l w)
;  (cond
;    [(empty? w) (list (list l))]
;    [(cons? w)
;     (cons
;      (cons l w)
;      (prepend (first w)
;               (insert-everywhere l (rest w))))]))
;
;(check-expect (insert-everywhere "d" '())
;             (list (list "d")))
;
;(check-expect (insert-everywhere "d" (list "e" "r"))
;              (list (list "d" "e" "r")
;                    (list "e" "d" "r")
;                    (list "e" "r" "d")))
;
;; 1String List-of-words -> List-of-words
;; Prepend l to all words in low.
;(define (prepend l low)
;  (cond
;    [(empty? low) '()]
;    [(cons? low)
;     (cons (cons l (first low))
;           (prepend l (rest low)))]))
;
;(check-expect (prepend "e" '())
;              '())
;
;(check-expect (prepend "e"
;                       (list
;                        (list "d" "r")
;                        (list "r" "d")))
;              (list
;               (list "e" "d" "r")
;               (list "e" "r" "d")))
;
;(check-expect (prepend "a"
;                       (list
;                        (list "b")
;                        (list "b" "c")
;                        (list "b" "c" "d")))
;              (list
;               (list "a" "b")
;               (list "a" "b" "c")
;               (list "a" "b" "c" "d")))