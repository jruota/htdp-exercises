;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex409) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; NorFalse is one of
; – N
; – #false

; N is one of:
; – 0
; – (add1 N)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define DB-ERROR "no such labels")

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

(define reordered-schema
  `(("Age" ,integer?)
    ("Present" ,boolean?)
    ("Name" ,string?)))

(define reordered-content
  `((35 #true "Alice")
    (25 #false "Bob")
    (30 #true "Carol")
    (32 #false "Dave")))
 
(define reordered-db
  (make-db reordered-schema reordered-content))

(define reordered-schema2
  `(("Age" ,integer?)
    ("Name" ,string?)))

(define reordered-content2
  `((35 "Alice")
    (25 "Bob")
    (30 "Carol")
    (32 "Dave")))

(define reordered-db2
  (make-db reordered-schema2 reordered-content2))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; DB [List-of Label] -> DB
; Return a database like db but with
; its columns reordered according to lol.
(define (reorder db lol)
  (local ((define schema (db-schema db))
          (define content (db-content db))
          (define labels (map (lambda (s) (first s)) schema))
          (define label-indices
            (filter (lambda (x) (integer? x))
                    (map (lambda (l) (index-of labels l)) lol)))
          (define reordered-schema
            (map (lambda (i) (list-ref schema i)) label-indices))

          ; Row -> Row
          ; Reorder row r according to lol.
          (define (reorder-row r)
            (map (lambda (i) (list-ref r i)) label-indices)))
    ; – IN –
    (if (empty? label-indices)
        (error DB-ERROR)
        (make-db reordered-schema (map reorder-row content)))))

(check-expect (db-content (reorder school-db (list "Age" "Present" "Name")))
              (db-content reordered-db))
(check-expect (db-content (reorder school-db (list "Age" "Name")))
              (db-content reordered-db2))
(check-expect (db-content
               (reorder school-db (list "Height" "Age" "Gender" "Name")))
              (db-content reordered-db2))
(check-error (reorder school-db (list "Height" "Gender" "Class"))
             DB-ERROR)

; [List-of X] X -> NorFalse
; Return the index of v in lst;
; if there is no v in lst, return #false.
(define (index-of lst v)
  (local (; [List-of X] N -> NorFalse
          ; Return the index of v in lst;
          ; if there is no v in lst, return #false.
          (define (main lst0 n)
            (cond
              [(empty? lst0) #false]
              [else
               (if (equal? v (first lst0))
                   n
                   (main (rest lst0) (add1 n)))])))
    ; – IN –
    (main lst 0)))

(check-expect (index-of '() 5)
              #false)
(check-expect (index-of (list 1 2 3 4 5 6 7 8 9 10) 5)
              4)
(check-expect (index-of (list 1 2 3 4 5 6 7 8 9 10) 12)
              #false)