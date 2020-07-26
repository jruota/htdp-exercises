;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex130) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-names is one of: 
; – '()
; – (cons String List-of-names)
; interpretation a list of invitees, by last name

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(cons "Armstrong"
      (cons "Lato"
            (cons "Funaki"
                  (cons "Gagarin"
                        (cons "Lewandowski" '())))))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(cons "1" (cons "2" '()))

; This is an element of List-of-names as it is a list containing only strings.

(cons 2 '())

; This is not an element of List-of-names as it contains a number but
; any element of List-of-names must contain Strings only.