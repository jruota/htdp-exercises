;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex408) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct db [schema content])
; A DB is a structure: (make-db Schema Content)
 
; A Schema is a [List-of Spec]
; A Spec is a [List Label Predicate]
; A Label is a String
; A Predicate is a [Any -> Boolean]
 
; A (piece of) Content is a [List-of Row]
; A Row is a [List-of Cell]
; A Cell is Any
; constraint cells do not contain functions 
 
; integrity constraint In (make-db sch con), 
; for every row in con,
; (I1) its length is the same as sch's, and
; (I2) its ith Cell satisfies the ith Predicate in sch

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define school-schema
  `(("Name"    ,string?)
    ("Age"     ,integer?)
    ("Present" ,boolean?)))

(define school-content
  `(("Alice" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)))

(define school-db
  (make-db school-schema
           school-content))

(define projected-content
  `(("Alice" #true)
    ("Carol" #true)))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; DB [List-of Label] [Row -> Boolean] -> [List-of Row]
; Return a list of rows that satisfy the given predicate pred,
; projected down to the given set of labels.
(define (select db lol pred)
  (filter pred (db-content (project db lol))))

(check-expect (select school-db
                      (list "Name" "Present")
                      (lambda (x) (last x)))
              projected-content)

; [List-of X] -> X
; Return the last element of l.
; Raise an error if l is empty.
(define (last l)
  (local (; [NEList-of X] -> X
          ; Return the last element of l.
          (define (ne-last l)
            (cond
              [(empty? (rest l)) (first l)]
              [else (ne-last (rest l))])))
    ; – IN –
    (cond
      [(empty? l)
       (error "expected a list of length greater than 0, empty given instead")]
      [else (ne-last l)])))

(check-error (last '())
             "expected a list of length greater than 0, empty given instead")
(check-expect (last (list 1))
              1)
(check-expect (last (list 1 2 3 4 5 6 7 8 9 10))
              10)

; from figure 142 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (project db labels)
  (local ((define schema  (db-schema db))
          (define content (db-content db))
 
          ; Spec -> Boolean
          ; does this column belong to the new schema
          (define (keep? c)
            (member? (first c) labels))

          (define mask (map keep? schema))
 
          ; Row -> Row 
          ; retains those columns whose name is in labels
          (define (row-project row)
            (foldr (lambda (cell m c) (if m (cons cell c) c))
                   '()
                   row
                   mask)))
    ; – IN –
    (make-db (filter keep? schema)
             (map row-project content))))