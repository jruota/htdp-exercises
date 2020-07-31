;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex145) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An NEList-of-temperatures is one of: 
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)
; interpretation non-empty lists of Celsius temperatures

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; NEList-of-temperatures -> Boolean
; Return #true if the temperatures are
; sorted in descending order, #false else.
(define (sorted>? nelot)
  (cond
    [(empty? (rest nelot)) #true]
    [else
     (and (>= (first nelot) (second nelot))
          (sorted>? (rest nelot)))]))

(check-expect (sorted>? (cons 1 '()))
              #true)
(check-expect (sorted>? (cons 1 (cons 2 '())))
              #false)
(check-expect (sorted>? (cons 2 (cons 1 '())))
              #true)
(check-expect (sorted>? (cons 10
                              (cons 9
                                    (cons 6
                                          (cons 4
                                                (cons -200
                                                      (cons -270 '())))))))
              #true)
(check-expect (sorted>? (cons 10
                              (cons 9
                                    (cons 6
                                          (cons 4
                                                (cons -200
                                                      (cons 270 '())))))))
              #false)
(check-expect (sorted>? (cons 1 (cons 1 (cons 1 (cons 1 (cons 1 '()))))))
              #true)