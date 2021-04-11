;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex348) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A BSL-val is one of:
; – Number
; – Boolean
; – String
; – Image

(define-struct AND [one two])
; An AND is structure:
;     (make-AND BSL-bool BSL-bool)
(define-struct OR [one two])
; An OR is structure:
;     (make-OR BSL-bool BSL-bool)
(define-struct NOT [one])
; A NOT is structure:
;     (make-NOT BSL-bool)

; A BSL-bool is one of:
; – #false
; – #true
; – AND
; – OR
; – NOT

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; BSL-bool -> BSL-val
; Compute the value of bslb.
(define (eval-bool-expression bslb0)
  (local (; BSL-bool -> BSL-val
          ; Compute the value of bslb1.
          (define (eval-bslb bslb1)
            (cond
              [(boolean? bslb1) bslb1]
              [(AND? bslb1) (eval-and bslb1)]
              [(OR? bslb1) (eval-or bslb1)]
              [(NOT? bslb1) (eval-not bslb1)]))

          ; AND -> BSL-val
          ; Compute the value of a.
          (define (eval-and a)
            (and (eval-bslb (AND-one a))
                 (eval-bslb (AND-two a))))

          ; OR -> BSL-val
          ; Compute the value of o.
          (define (eval-or o)
            (or (eval-bslb (OR-one o))
                (eval-bslb (OR-two o))))

          ; NOT -> BSL-val
          ; Compute the value of n.
          (define (eval-not n)
            (not (eval-bslb (NOT-one n)))))
    ; – IN –
    (eval-bslb bslb0)))

(check-expect (eval-bool-expression #true)
              #true)
(check-expect (eval-bool-expression #false)
              #false)
(check-expect (eval-bool-expression (make-AND #true #true))
              #true)
(check-expect (eval-bool-expression (make-AND #true #false))
              #false)
(check-expect (eval-bool-expression (make-OR #false #true))
              #true)
(check-expect (eval-bool-expression (make-OR #false #false))
              #false)
(check-expect (eval-bool-expression (make-NOT #false))
              #true)
(check-expect (eval-bool-expression (make-NOT #true))
              #false)
(check-expect (eval-bool-expression
               (make-AND #true
                    (make-OR #false
                        (make-NOT (make-OR #false #false)))))
              #true)