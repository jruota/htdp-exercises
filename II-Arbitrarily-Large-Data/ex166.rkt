;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex166) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct work [employee rate hours])
; A (piece of) Work is a structure: 
;   (make-work String Number Number)
; interpretation (make-work n r h) combines the name 
; with the pay rate r and the number of hours h

; Low (short for list of works) is one of: 
; – '()
; – (cons Work Low)
; interpretation an instance of Low represents the 
; hours worked for a number of employees

(define-struct paycheck [name pay])
; A Paycheck is a structure:
;     (make-paycheck String Number)
; Interpretation:
;     Combines the employee's name and
;     his or her weekly pay.

; A List-of-Paychecks is one of:
; – '()
; – (cons Paycheck List-of-Paychecks)
; Interpretation:
;     A collection of employees and
;     their weekly pay.

; ------------------------------------------------------------------------------

(define-struct ei [name number])
; An EI (employee information) is a structure:
;     (make-ei String String)
; Interpretation:
;     Contains the name and employee number.

(define-struct rw [ei rate hours])
; A RW (revised work) is a structure:
;     (make-revised-work EI Number Number)
; Interpretation:
;     Combines the employee information
;     with the pay rate and the number
;     of hours worked.

(define-struct rp [name number pay])
; A RP (revised paycheck) is a structure:
;     (make-rp String String Number)
; Interpretation:
;     Represents a paycheck with the employee's
;     name, the employee number and the weekly pay.

; A List-of-RW is one of
; – '()
; – (cons RW List-of-RW)
; Interpretation:
;     A list containing the names,
;     employee numbers and hours
;     worked for a number of employees.

; A List-of-RP is one of:
; – '()
; – (cons RP List-of-RP)
; Interpretation:
;     A list containing the paychecks
;     of a number of employees.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; List-of-RW -> List-of-RP
; Calculate the (revised) paychecks
; of all employees in lorw.
(define (wage*.v4 lorw)
  (cond
    [(empty? lorw) '()]
    [(cons? lorw)
     (cons (make-rp (get-name (first lorw))
                    (get-number (first lorw))
                    (wage.v4 (first lorw)))
           (wage*.v4 (rest lorw)))]))

(check-expect (wage*.v4 '())
              '())
(check-expect (wage*.v4 (cons (make-rw (make-ei "Robby" "0337") 11.95 39) '()))
              (cons (make-rp "Robby" "0337" (* 11.95 39)) '()))
(check-expect (wage*.v4
               (cons (make-rw (make-ei "Robby" "0337") 11.95 39)
                     (cons (make-rw (make-ei "Matthew" "0177") 12.95 45) '())))
              (cons (make-rp "Robby" "0337" (* 11.95 39))
                    (cons (make-rp "Matthew" "0177" (* 12.95 45)) '())))

; Low -> List-of-Paychecks
; Calculate the paychecks of all
; employees in low.
(define (wage*.v3 low)
  (cond
    [(empty? low) '()]
    [(cons? low)
     (cons (make-paycheck (work-employee (first low))
                          (wage.v2 (first low)))
           (wage*.v3 (rest low)))]))

(check-expect (wage*.v3 '())
              '())
(check-expect (wage*.v3 (cons (make-work "Robby" 11.95 39) '()))
              (cons (make-paycheck "Robby" (* 11.95 39)) '()))
(check-expect (wage*.v3 (cons (make-work "Matthew" 12.95 45)
                              (cons (make-work "Robby" 11.95 39) '())))
              (cons (make-paycheck "Matthew" (* 12.95 45))
                    (cons (make-paycheck "Robby" (* 11.95 39)) '())))
(check-expect (wage*.v3
               (cons (make-work "Dillon" 11 25)
                     (cons (make-work "Burak" 10 40)
                           (cons (make-work "Mike" 12.50 40)
                                 (cons (make-work "Julian" 14 60) '())))))
              (cons (make-paycheck "Dillon" (* 11 25))
                    (cons (make-paycheck "Burak" (* 10 40))
                          (cons (make-paycheck "Mike" (* 12.50 40))
                                (cons (make-paycheck "Julian" (* 14 60))
                                      '())))))
(check-expect (wage*.v3
               (cons (make-work "Dillon" 11 25)
                     (cons (make-work "Burak" 10 40)
                           (cons (make-work "Mike" 12.50 40)
                                 (cons (make-work "Julian" 14 60)
                                       (cons (make-work "Katja" 11 30) '()))))))
              (cons (make-paycheck "Dillon" (* 11 25))
                    (cons (make-paycheck "Burak" (* 10 40))
                          (cons (make-paycheck "Mike" (* 12.50 40))
                                (cons (make-paycheck "Julian" (* 14 60))
                                      (cons (make-paycheck "Katja" (* 11 30))
                                            '()))))))

; RW -> Number
; Compute the wage for the give
; revised word record rw.
(define (wage.v4 rw)
  (* (rw-rate rw) (rw-hours rw)))

(check-expect (wage.v4 (make-rw "Robby" 11.95 39))
              (* 11.95 39))

; Work -> Number
; computes the wage for the given work record w
(define (wage.v2 w)
  (* (work-rate w) (work-hours w)))

(check-expect (wage.v2 (make-work "Robby" 11.95 39))
              (* 11.95 39))

; RW -> String
; Return the name of the employee.
(define (get-name rw)
  (ei-name (rw-ei rw)))

(check-expect (get-name (make-rw (make-ei "Robby" "0337") 11.95 39))
              "Robby")

; RW -> String
; Return the employee number.
(define (get-number rw)
  (ei-number (rw-ei rw)))

(check-expect (get-number (make-rw (make-ei "Robby" "0337") 11.95 39))
              "0337")

;; RW -> EI
;; Extract the employee information
;; from rw.
;(define (get-ei rw)
;  (rw-ei rw))
;
;(check-expect (get-ei (make-rw (make-ei "Robby" "0337") 11.95 39))
;              (make-ei "Robby" "0337"))