;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex410) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define SCHEMA-ERROR "databases have different schemas")

(define school-schema
  `(("Name"    ,string?)
    ("Age"     ,integer?)
    ("Present" ,boolean?)))

(define school-schema2
  `(("Name"   ,string?)
    ("Age"    ,integer?)
    ("Passed" ,boolean?)))

(define school-content
  `(("Alice" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)))

(define school-content2
  `(("Alice"   35 #true)
    ("Caitlin" 39 #false)
    ("Seymour" 34 #true)
    ("Bob"     25 #false)
    ("Teresa"  38 #true)
    ("Carol"   30 #true)
    ("Larissa" 42 #true)
    ("Dave"    32 #false)
    ("Tyrone"  19 #true)))

(define school-content-joined
  `(("Alice" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)
    ("Caitlin" 39 #false)
    ("Seymour" 34 #true)
    ("Teresa"  38 #true)
    ("Larissa" 42 #true)
    ("Tyrone"  19 #true)))  

(define school-db
  (make-db school-schema school-content))

(define school-db2
  (make-db school-schema school-content2))

(define school-db3
  (make-db school-schema2 school-content2))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; NOTE ------------------------------------------------------------
; Assumes that the schemas agree on the predicates for each column.
; END NOTE --------------------------------------------------------

; DB DB -> DB
; Consume two databases with the exact same schema
; and produce a new database with this schema
; and the joint content of both.
; Eliminate rows with the exact same content.
(define (db-join db1 db2)
  (local ((define labels1 (map first (db-schema db1)))
          (define labels2 (map first (db-schema db2)))

          ; [List-of X] any -> Content
          ; Insert lox at the end of any.
          (define (insert-at-end lox any)
            (cond
              [(empty? lox) (list any)]
              [else
               (cons (first lox)
                     (insert-at-end (rest lox) any))]))

          ; Content Content -> Content
          ; Append content1 and content2,
          ; ignore rows from content2 that are
          ; already present in content1.
          (define (content-append content1 content2)
            (cond
              [(empty? content2) content1]
              [else
               (if (member? (first content2) content1)
                   (content-append content1 (rest content2))
                   (content-append (insert-at-end content1 (first content2))
                                   (rest content2)))])))
    ; – IN –
    (if (equal? labels1 labels2)
        (make-db (db-schema db1)
                 (content-append (db-content db1) (db-content db2)))
        (error SCHEMA-ERROR))))

(check-expect (db-content (db-join school-db school-db))
              school-content)
(check-expect (db-content (db-join school-db school-db2))
              school-content-joined)
(check-error (db-join school-db school-db3)
             SCHEMA-ERROR)