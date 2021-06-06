;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex397) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct tr [number hours])
; A TimeRecord is a structure:
;     (make-tr EmployeeNumber Number)
; Interpretation:
;     The employee number and the number of
;     hours worked per week by said employee.

(define-struct er [name number pay])
; An EmployeeRecord is a structure:
;     (make-er String EmployeeNumber Number)
; Interpretation:
;     The name of an employee, his/her employee number
;     and weekly wage.

(define-struct wr [name wage])
; A WageRecord is a structure:
;     (make-wr String Number)
; Interpretation:
;     The name and the weekly wage of an employee.

; An EmployeeNumber is a String of length 5
; containing the numerical strings 0 to 9.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define ER1 (make-er "Jimena" "64522" 15.4))
(define ER2 (make-er "Jon" "04569" 13.9))
(define ER3 (make-er "Bronson" "65898" 15.7))
(define ER4 (make-er "Angela" "24769" 9.6))

(define TR1 (make-tr "64522" 56))
(define TR2 (make-tr "04569" 43))
(define TR3 (make-tr "65898" 59))
(define TR4 (make-tr "24769" 53))

(define WR1 (make-wr (er-name ER1) (* (er-pay ER1) (tr-hours TR1))))
(define WR2 (make-wr (er-name ER2) (* (er-pay ER2) (tr-hours TR2))))
(define WR3 (make-wr (er-name ER3) (* (er-pay ER3) (tr-hours TR3))))
(define WR4 (make-wr (er-name ER4) (* (er-pay ER4) (tr-hours TR4))))

(define ERROR1 "Employee Record missing.")
(define ERROR2 "Time Record missing.")
(define ERROR3 "Employee and/or Time Record missing.")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; [List-of EmployeeRecord] [List-of TimeRecord] -> [List-of WageRecord]
; Calculate a list of WageRecords from er and tr.
(define (wage*.v3 er tr)
  (cond
    [(and (empty? er) (empty? tr)) '()]
    [(and (empty? er) (cons? tr)) (error ERROR1)]
    [(and (cons? er) (empty? tr)) (error ERROR2)]
    [(and (cons? er) (cons? tr))
     (if (string=? (er-number (first er)) (tr-number (first tr)))
         (cons
          (make-wr (er-name (first er))
                   (* (er-pay (first er)) (tr-hours (first tr))))
          (wage*.v3 (rest er) (rest tr)))
         (error ERROR3))]))

(check-expect (wage*.v3 '() '())
              '())
(check-expect (wage*.v3 (list ER1 ER2 ER3 ER4) (list TR1 TR2 TR3 TR4))
              (list WR1 WR2 WR3 WR4))
(check-error (wage*.v3 (list ER1 ER2 ER3) (list TR1 TR2 TR3 TR4))
             ERROR1)
(check-error (wage*.v3 (list ER1 ER2 ER3 ER4) (list TR1 TR2 TR3))
             ERROR2)
(check-error (wage*.v3 (list ER1 ER2 ER4 ER3) (list TR1 TR2 TR3 TR4))
             ERROR3)