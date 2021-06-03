;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex388) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct employee [name ssn pay-rate])
; An Employee is a structure:
;     (make-employee String String Number)
; Interpretation:
;     The name, his/her social security number and hourly pay rate.

(define-struct wr [name hours])
; A WorkRecord is a structure:
;     (make-wr String Number)
; Interpretation:
;     An employee's name and the number of hours worked in a week.

(define-struct wp [name amount])
; A WeeklyPay is a structure:
;     (make-wp String Number)
; Interpretation:
;     An employee's name and weekly pay.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; NOTE -------------------------------------------------------------------------

; The function wages*.v2 assumes that the lists have the same length and that
; corresponding entries belong to the same employee.

; END NOTE ---------------------------------------------------------------------

; [List-of Employee] [List-of WorkRecord] -> [List-of WeeklyPay]
; Calculate the weekly pay of the employees.
(define (wages*.v2 loe lowr)
  (cond
    [(empty? loe) '()]
    [else
     (cons
      (make-wp
       (employee-name (first loe))
       (weekly-wage (employee-pay-rate (first loe))
                    (wr-hours (first lowr))))
      (wages*.v2 (rest loe) (rest lowr)))]))

(check-expect (wages*.v2 '() '())
              '())
(check-expect (wages*.v2
               (list (make-employee "John Doe" "12345" 5.65)
                     (make-employee "Jane Doe" "67890" 8.75))
               (list (make-wr "John Doe" 40)
                     (make-wr "Jane Doe" 30)))
              (list (make-wp "John Doe" 226)
                    (make-wp "Jane Doe" 262.5)))

; Number Number -> Number
; Compute the weekly wage from pay-rate and hours.
(define (weekly-wage pay-rate hours)
  (* pay-rate hours))

(check-expect (weekly-wage 5.55 10)
              55.5)
