;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex187) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct gp [name score])
; A GamePlayer is a structure: 
;    (make-gp String Number)
; interpretation (make-gp p s) represents player p who 
; scored a maximum of s points

; A List-of-GamePlayers is one of:
; – '()
; – (cons GamePlayer List-of-GamePlayers)
; Interpretation:
;     A list containing GamePlayers.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; List-of-GamePlayers -> List-of-GamePlayers
; Sort logp by score in descending order.
(define (sort-gp> logp)
  (cond
    [(empty? logp) '()]
    [(cons? logp)
     (insert-gp (first logp) (sort-gp> (rest logp)))]))

(check-expect (sort-gp> '())
              '())
(check-expect (sort-gp> (list (make-gp "Julia" 239)
                              (make-gp "Andy" 143)
                              (make-gp "Jimena" 413)
                              (make-gp "Jamal" 408)
                              (make-gp "Robert" 3)))
              (list (make-gp "Jimena" 413)
                    (make-gp "Jamal" 408)
                    (make-gp "Julia" 239)
                    (make-gp "Andy" 143)
                    (make-gp "Robert" 3)))
(check-expect (sort-gp> (list (make-gp "Jimena" 413)
                              (make-gp "Jamal" 408)
                              (make-gp "Julia" 239)
                              (make-gp "Andy" 143)
                              (make-gp "Robert" 3)))
              (list (make-gp "Jimena" 413)
                    (make-gp "Jamal" 408)
                    (make-gp "Julia" 239)
                    (make-gp "Andy" 143)
                    (make-gp "Robert" 3)))
(check-expect (sort-gp> (list (make-gp "Robert" 3)
                              (make-gp "Andy" 143)
                              (make-gp "Julia" 239)
                              (make-gp "Jamal" 408)
                              (make-gp "Jimena" 413)))
              (list (make-gp "Jimena" 413)
                    (make-gp "Jamal" 408)
                    (make-gp "Julia" 239)
                    (make-gp "Andy" 143)
                    (make-gp "Robert" 3)))

; GamePlayer List-of-GamePlayers -> List-of-GamePlayers
; Insert gp in the sorted (in descending order)
; list logp.
(define (insert-gp gp logp)
  (cond
    [(empty? logp) (list gp)]
    [(cons? logp)
     (if (score>=? gp (first logp))
         (cons gp logp)
         (cons (first logp) (insert-gp gp (rest logp))))]))

(check-expect (insert-gp (make-gp "Julia" 239) '())
              (list (make-gp "Julia" 239)))
(check-expect (insert-gp (make-gp "Julia" 239)
                         (list (make-gp "Jimena" 413)
                               (make-gp "Jamal" 408)
                               (make-gp "Andy" 143)
                               (make-gp "Robert" 3)))
              (list (make-gp "Jimena" 413)
                    (make-gp "Jamal" 408)
                    (make-gp "Julia" 239)
                    (make-gp "Andy" 143)
                    (make-gp "Robert" 3)))

; GamePlayer GamePlayer -> Boolean
; Is the scor of gp1 greater than or equal to
; the score of gp2?
(define (score>=? gp1 gp2)
  (>= (gp-score gp1) (gp-score gp2)))

(check-expect (score>=? (make-gp "Karolina" 111)
                        (make-gp "Filip" 111))
              #true)
(check-expect (score>=? (make-gp "Karolina" 123)
                        (make-gp "Filip" 456))
              #false)
(check-expect (score>=? (make-gp "Karolina" 789)
                        (make-gp "Filip" 234))
              #true)