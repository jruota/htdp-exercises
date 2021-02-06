;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex303) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Draw arrows from the shaded occurrences of x to their binding occurrences in
; each of the following three lambda expressions.

; Use DrRacket's "Check Syntax".

; Also draw a box for the scope of each shaded x and holes in the scope
; as necessary.

; The scopes and their holes become apparent after using "Check Syntax".

(lambda (x y)
  (+ x (* x y)))

(lambda (x y)
  (+ x
     (local ((define x (* y y)))
       (+ (* 3 x)
          (/ 1 x)))))

(lambda (x y)
  (+ x
     ((lambda (x)
        (+ (* 3 x)
           (/ 1 x)))
      (* y y))))
