;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex348) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct AND [left right])
; An AND is structure:
;     (make-AND BSL-bool BSL-bool)
(define-struct OR [left right])
; An OR is structure:
;     (make-OR BSL-bool BSL-bool)
(define-struct NOT [boolval])
; A NOT is structure:
;     (make-NOT BSL-bool)

; A BSL-bool is one of:
; – #false
; – #true
; – AND
; – OR
; – NOT

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-bool -> Boolean
; Compute the value of the representation
; of Boolean BSL expression bslb.
(define (eval-bool-expression bslb)
  (cond
    [(boolean? bslb) bslb]
    [(AND? bslb)
     (and (eval-bool-expression (AND-left bslb))
          (eval-bool-expression (AND-right bslb)))]
    [(OR? bslb)
     (or (eval-bool-expression (OR-left bslb))
         (eval-bool-expression (OR-right bslb)))]
    [(NOT? bslb)
     (not (eval-bool-expression (NOT-boolval bslb)))]))

(check-expect (eval-bool-expression #true) #true)
(check-expect (eval-bool-expression #false) #false)

(check-expect (eval-bool-expression (make-AND #true #true)) #true)
(check-expect (eval-bool-expression (make-AND #true #false)) #false)
(check-expect (eval-bool-expression (make-AND #false #true)) #false)
(check-expect (eval-bool-expression (make-AND #false #false)) #false)

(check-expect (eval-bool-expression (make-OR #true #true)) #true)
(check-expect (eval-bool-expression (make-OR #true #false)) #true)
(check-expect (eval-bool-expression (make-OR #false #true)) #true)
(check-expect (eval-bool-expression (make-OR #false #false)) #false)

(check-expect (eval-bool-expression (make-NOT #true)) #false)
(check-expect (eval-bool-expression (make-NOT #false)) #true)

(check-expect (eval-bool-expression
               (make-NOT
                (make-OR #false
                         (make-AND #true
                                   (make-NOT #false)))))
              #false)
