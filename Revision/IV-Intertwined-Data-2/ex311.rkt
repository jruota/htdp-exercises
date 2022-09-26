;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex311) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct no-parent [])
(define NP (make-no-parent))
(define-struct child [father mother name date eyes])

; An FT (short for family tree) is one of: 
; – NP
; – (make-child FT FT String N String)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define CARL (make-child NP NP "Carl" 1926 "green"))
(define BETTINA (make-child NP NP "Bettina" 1926 "green"))

(define FRED (make-child NP NP "Fred" 1966 "pink"))

(define ADAM (make-child CARL BETTINA "Adam" 1950 "hazel"))
(define DAVE (make-child CARL BETTINA "Dave" 1955 "black"))
(define EVA (make-child CARL BETTINA "Eva" 1965 "blue"))

(define GUSTAV (make-child FRED EVA "Gustav" 1988 "brown"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; FT Number -> Number
; Given the family tree ft the the current year cy,
; return the average age of all child structures in ft.
(define (average-age ft cy)
  (cond
    [(no-parent? ft) 0]
    [else
     (local (; FT -> Number
             ; Return the sum of the ages of
             ; all child structures in ft0.
             (define (sum-of-ages ft0)
               (cond
                 [(no-parent? ft0) 0]
                 [else
                  (+ (- cy (child-date ft0))
                     (sum-of-ages (child-father ft0))
                     (sum-of-ages (child-mother ft0)))])))
       ; – IN –
       (/ (sum-of-ages ft) (count-persons ft)))]))

(check-expect (average-age NP 2022) 0)
(check-expect (average-age BETTINA 2022) 96)
(check-expect (average-age EVA 2022) 83)
(check-expect (average-age GUSTAV 2022) 67.8)

; FT Number -> Number
; Given the family tree ft the the current year cy,
; return the average age of all child structures in ft.
(define (average-age.v2 ft cy)
  (local ((define NUMBER-OF-PEOPLE (count-persons ft))
          
          ; FT -> Number
          ; Return the sum of the ages of
          ; all child structures in ft0.
          (define (sum-of-ages ft0)
            (cond
              [(no-parent? ft0) 0]
              [else
               (+ (- cy (child-date ft0))
                  (sum-of-ages (child-father ft0))
                  (sum-of-ages (child-mother ft0)))])))
    ; – IN –
    (cond
      [(zero? NUMBER-OF-PEOPLE) 0]
      [else
       (/ (sum-of-ages ft) NUMBER-OF-PEOPLE)]))) 

(check-expect (average-age.v2 NP 2022) 0)
(check-expect (average-age.v2 BETTINA 2022) 96)
(check-expect (average-age.v2 EVA 2022) 83)
(check-expect (average-age.v2 GUSTAV 2022) 67.8)

; NOTE --------------------------------------------
; This version traverses the family tree only once,
; not twice like the previous versions.
; END NOTE ----------------------------------------
; FT Number -> Number
; Given the family tree ft the the current year cy,
; return the average age of all child structures in ft.
(define (average-age.v3 ft cy)
  (local ((define-struct ftac [soa noc])
          ; An FTAC (FamilyTreeAgesCount) is a
          ; (make-ft-ages-count Number Number).
          ; Interpretation:
          ;   Collect the sum of ages and the number
          ;   of child-structures in a given FT.

          ; FT -> FTAC
          ; Return the sum of all ages and the number
          ; of child-structures in ft0.
          (define (get-ftac ft0)
            (cond
              [(no-parent? ft0)
               (make-ftac 0 0)]
              [else
               (local ((define FATHER (get-ftac (child-father ft0)))
                       (define MOTHER (get-ftac (child-mother ft0))))
                 ; – IN –
                 (make-ftac (+ (- cy (child-date ft0))
                               (ftac-soa FATHER)
                               (ftac-soa MOTHER))
                            (+ 1
                               (ftac-noc FATHER)
                               (ftac-noc MOTHER))))])))
    ; – IN –
    (local ((define FTAC (get-ftac ft))
            (define SOA (ftac-soa FTAC))
            (define NOC (ftac-noc FTAC)))
      ; – IN –
      (cond
        [(zero? NOC) 0]
        [else
         (/ SOA NOC)]))))

(check-expect (average-age.v3 NP 2022) 0)
(check-expect (average-age.v3 BETTINA 2022) 96)
(check-expect (average-age.v3 EVA 2022) 83)
(check-expect (average-age.v3 GUSTAV 2022) 67.8)

; from ex310.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; FT -> Number
; Count the child structures in ft.
(define (count-persons ft)
  (cond
    [(no-parent? ft) 0]
    [else
     (+ 1
        (count-persons (child-father ft))
        (count-persons (child-mother ft)))]))
