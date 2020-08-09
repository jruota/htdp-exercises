;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex177) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor Lo1S Lo1S) 
; An Lo1S is one of: 
; – '()
; – (cons 1String Lo1S)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; String String -> Editor
; Create an editor from s1 and s2.
(define (create-editor s1 s2)
  (make-editor (rev (explode s1))
               (explode s2)))

(check-expect (create-editor "hello" "world")
              (make-editor (cons "o"
                                 (cons "l"
                                       (cons "l"
                                             (cons "e"
                                                   (cons "h" '())))))
                           (cons "w"
                                 (cons "o"
                                       (cons "r"
                                             (cons "l"
                                                   (cons "d" '())))))))

; Lo1s -> Lo1s 
; produces a reverse version of the given list 
(define (rev l)
  (cond
    [(empty? l) '()]
    [else (add-at-end (rev (rest l)) (first l))]))

(check-expect (rev '())
              '())
(check-expect (rev (cons "a" (cons "b" (cons "c" '()))))
              (cons "c" (cons "b" (cons "a" '()))))

; Lo1s 1String -> Lo1s
; creates a new list by adding s to the end of l
(define (add-at-end l s)
  (cond
    [(empty? l) (cons s '())]
    [else
     (cons (first l) (add-at-end (rest l) s))]))

(check-expect (add-at-end '() "a")
              (cons "a" '()))
(check-expect (add-at-end (cons "c" (cons "b" '())) "a")
              (cons "c" (cons "b" (cons "a" '()))))