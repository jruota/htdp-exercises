;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex165) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A TD (toy description) is a one-word String.

; A TDL (toy description list) is one of:
; – '()
; – (cons TD TDL)
; Interpretation:
;     A list containing toy descriptions.

; A List-of-Strings is one of:
; – '()
; – (cons String List-of-Strings)
; Interpretation:
;     The collection of all lists
;     containing Strings.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; String String List-of-Strings -> List-of-Strings
; Substite all occurrences of old in los with new.
(define (substitute new old los)
  (cond
    [(empty? los) '()]
    [(cons? los)
     (cons (if (string=? (first los) old)
               new
               (first los))
           (substitute new old (rest los)))]))

(check-expect (substitute "fizz"
                          "buzz"
                          (cons "chocolate"
                                (cons "buzz"
                                      (cons "bumblebee"
                                            (cons "buzz"
                                                  (cons "Jupiter"
                                                        (cons "buzz" '())))))))
              (cons "chocolate"
                    (cons "fizz"
                          (cons "bumblebee"
                                (cons "fizz"
                                      (cons "Jupiter"
                                            (cons "fizz" '())))))))

; TDL -> TDL
; Replace all occurrences of "robot" with "r2d2",
; leave all other descriptions the same.
(define (subst-robot tdl)
  (cond
    [(empty? tdl) '()]
    [(cons? tdl)
     (cons (robot->r2d2 (first tdl))
           (subst-robot (rest tdl)))]))

(check-expect (subst-robot '())
              '())
(check-expect (subst-robot (cons "car"
                                 (cons "puppet"
                                       (cons "robot"
                                             (cons "space-ship"
                                                   (cons "robot" '()))))))
              (cons "car"
                    (cons "puppet"
                          (cons "r2d2"
                                (cons "space-ship"
                                      (cons "r2d2" '()))))))

; TD -> TD
; If td is equal to "robot", replace it
; with "r2d2", leave it as it is otherwise.
(define (robot->r2d2 td)
  (if (string=? td "robot")
      "r2d2"
      td))

(check-expect (robot->r2d2 "fire-truck")
              "fire-truck")
(check-expect (robot->r2d2 "robot")
              "r2d2")