;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex184) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(check-expect (list #false #false)
              (list (string=? "a" "b") #false))

(check-expect (list 30 200 1/2)
              (list (+ 10 20) (* 10 20) (/ 10 20)))

(check-expect (cons "dana"
                    (cons "jane"
                          (cons "mary"
                                (cons "laura" '()))))
              (list "dana" "jane" "mary" "laura"))