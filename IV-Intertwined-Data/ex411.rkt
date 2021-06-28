;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex411) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require racket/list)

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

(define presence-schema
  `(("Present"     ,boolean?)
    ("Description" ,string?)))

(define joined-schema
  `(("Name"        ,string?)
    ("Age"         ,integer?)
    ("Description" ,string?)))

(define school-content
  `(("Alice" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)))

(define presence-content
  `((#true  "presence")
    (#false "absence")))

(define extended-presence-content
  `((#true  "presence")
    (#true  "here")
    (#false "absence")
    (#false "there")))

(define joined-content
  `(("Alice" 35 "presence")
    ("Bob"   25 "absence")
    ("Carol" 30 "presence")
    ("Dave"  32 "absence")))

(define extended-joined-content
  `(("Alice" 35 "presence")
    ("Alice" 35 "here")
    ("Bob"   25 "absence")
    ("Bob"   25 "there")
    ("Carol" 30 "presence")
    ("Carol" 30 "here")
    ("Dave"  32 "absence")
    ("Dave"  32 "there")))  

(define school-db
  (make-db school-schema school-content))

(define presence-db
  (make-db presence-schema presence-content))

(define extended-presence-db
  (make-db presence-schema extended-presence-content))

(define extended-joined-presence-db
  (make-db presence-schema extended-joined-content))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; NOTE ----------------------------------------------------------
; Assumes that the schema of db-2 starts with the exact same Spec
; that the schema of db-1 ends in.
; END NOTE ------------------------------------------------------

; DB DB -> DB
; Create a database from db-1 by replacing the last cell in each row
; with the translation of the cell in db-2.
(define (join db-1 db-2)
  (local ((define db-1-schema (db-schema db-1))
          (define db-2-schema (db-schema db-2))
          (define db-1-content (db-content db-1))
          (define db-2-content (db-content db-2))

          ; Row Content -> Row
          ; Translate the last column of row using cont
          ; if its value matches the first row of cont.
          (define (translate-row row cont)
            (cond
              [(empty? cont) '()]
              [else
               (if (equal? (last row) (first (first cont)))
                   (cons (replace-last row (last (first cont)))
                         (translate-row row (rest cont)))
                   (translate-row row (rest cont)))])))
    ; – IN –
    (make-db (replace-last db-1-schema (last db-2-schema))
             (foldr
              append
              '()
              (map (lambda (r) (translate-row r db-2-content)) db-1-content)))))

(check-expect (db-content (join school-db presence-db))
              joined-content)
(check-expect (db-content (join school-db extended-presence-db))
              extended-joined-content)

; [List-of X] Y -> [List-of Z]
; Replace the last element of lox with y.
(define (replace-last lox y)
  (local (; [NEList-of X] -> [List-of Z]
          ; Replace the last element of the
          ; non-empty list lox0 with y.
          (define (ne-replace-last lox0)
            (cond
              [(empty? (rest lox0)) (list y)]
              [else (cons (first lox0) (ne-replace-last (rest lox0)))])))
    ; – IN –
    (if (empty? lox)
        (list y)
        (ne-replace-last lox))))

(check-expect (replace-last '() 10)
              (list 10))
(check-expect (replace-last (list 1) 11)
              (list 11))
(check-expect (replace-last (list 'one 'two) "three")
              (list 'one "three"))
(check-expect (replace-last (list 1 2 3 4 5 6 7 8 9 10) "marmelade")
              (list 1 2 3 4 5 6 7 8 9 "marmelade"))