;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex135) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-names is one of: 
; – '()
; – (cons String List-of-names)
; interpretation a list of invitees, by last name

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; List-of-names -> Boolean
; determines whether "Flatt" occurs on alon
(define (contains-flatt? alon)
  (cond
    [(empty? alon) #false]
    [(cons? alon)
     (or (string=? (first alon) "Flatt")
         (contains-flatt? (rest alon)))]))

(check-expect
  (contains-flatt? (cons "X" (cons "Y"  (cons "Z" '()))))
  #false)
(check-expect
  (contains-flatt? (cons "A" (cons "Flatt" (cons "C" '()))))
  #true)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(contains-flatt? (cons "Flatt" (cons "C" '())))

; Returns #true, as the first element is "Flatt".

(contains-flatt? (cons "A" (cons "Flatt" (cons "C" '()))))

; Returns #true, as the second element is "Flatt".

(contains-flatt? (cons "A" (cons "B" (cons "C" '()))))

; Returns #false, as none of the elements is "Flatt".
; "contains-flatt?" ends up calling itself three times:
;     – the first time with (cons "B" (cons "C" '())) as argument,
;     – the second time with (cons "C" '()) as argument,
;     – the third and last time with '() as argument.