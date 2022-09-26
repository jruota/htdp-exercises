;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex315) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct no-parent [])
(define NP (make-no-parent))
(define-struct child [father mother name date eyes])

; An FT (short for family tree) is one of: 
; – NP
; – (make-child FT FT String N String)

; An FF (short for family forest) is a [List-of FT].
; Interpretation:
;     A family forest represents several
;     families (say, a town) and their ancestor trees.

; An N is one of: 
; – 0
; – (add1 N)
; Interpretation:
;     Represents the counting numbers.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define CARL (make-child NP NP "Carl" 1926 "green"))
(define BETTINA (make-child NP NP "Bettina" 1926 "green"))

(define FRED (make-child NP NP "Fred" 1966 "pink"))

(define ADAM (make-child CARL BETTINA "Adam" 1950 "hazel"))
(define DAVE (make-child CARL BETTINA "Dave" 1955 "black"))
(define EVA (make-child CARL BETTINA "Eva" 1965 "blue"))

(define GUSTAV (make-child FRED EVA "Gustav" 1988 "brown"))

(define FF1 (list CARL BETTINA))
(define FF2 (list FRED EVA))
(define FF3 (list FRED EVA CARL))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; NOTE ------------------------------------------
; Parts of this function are taken from ex311.rkt
; END NOTE --------------------------------------

; FF N -> N
; Given a family forest ff and a year y, calculate
; the average age of all child instances in the
; forest in the given year.
(define (average-age ff y)
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
                 (make-ftac (+ (- y (child-date ft0))
                               (ftac-soa FATHER)
                               (ftac-soa MOTHER))
                            (+ 1
                               (ftac-noc FATHER)
                               (ftac-noc MOTHER))))]))

          ; FTAC FTAC -> FTAC
          ; Add the values of the corresponding fields
          ; in ftac1 and ftac2.
          (define (accumulate-ftac ftac1 ftac2)
            (make-ftac (+ (ftac-soa ftac1)
                          (ftac-soa ftac2))
                       (+ (ftac-noc ftac1)
                          (ftac-noc ftac2)))))
    ; – IN –
    (cond
      [(empty? ff) 0]
      [else
       (local ((define FTAC
                 (foldl accumulate-ftac
                        (make-ftac 0 0)
                        (map get-ftac ff)))
               (define SOA (ftac-soa FTAC))
               (define NOC (ftac-noc FTAC)))
         ; – IN –
         (cond
           [(zero? NOC) 0]
           [else
            (/ SOA NOC)]))])))

(check-expect (average-age '() 2022) 0)
(check-expect (average-age (list NP NP NP NP) 2022) 0)
(check-expect (average-age FF1 2022) 96)
(check-expect (average-age FF2 2022) (/ (+ 56 57 96 96) 4))
(check-expect (average-age FF3 2022) (/ (+ 56 57 96 96 96) 5))
