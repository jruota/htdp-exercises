;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex234) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A LOS (list of songs) is one of:
;     – '()
;     – (cons String LOS)
; Interpretation:
;     A list of songs.

; A RS (ranked song) is a list:
;     (list Number String)
; Interpretation:
;     The rank of a song and its title.

; A LORS (list of ranked songs) is one of:
;     – '()
;     – (cons RS LORS)
; Interpretation:
;     A list of ranked songs.

; A LRHTD (list representation of HTML table data)
; is a list representation of HTML table data.

; A LRHTR (list representation of an HTML table row)
; is a list representation of an HTML table row.

; A LoLRHTR (list of LRHTR) is one of:
;     – '()
;     – (cons LRHTR LoLRHTR)
; Interpretation:
;     A collection of list representations of
;     HTML table rows.

; A LRHT (list representation of HTML tables)
; is a nested list representation of HTML tables.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define ONE-LIST
  '("Asia: Heat of the Moment"
    "U2: One"
    "The White Stripes: Seven Nation Army"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; LOS -> LRHT
; Create a list representation of HTML tables.
(define (make-ranking los)
  `(table ((border "1"))
          ,@(make-table (ranking los))))

(check-expect (make-ranking '())
              '(table ((border "1"))))
(check-expect (make-ranking ONE-LIST)
              '(table ((border "1"))
                      (tr (td "1")
                          (td "Asia: Heat of the Moment"))
                      (tr (td "2")
                          (td "U2: One"))
                      (tr (td "3")
                          (td "The White Stripes: Seven Nation Army"))))

; LORS -> LoLRHTR
; Create a collection, i.e. list, of list representations
; of HTML table rows.
(define (make-table lors)
  (cond
    [(empty? lors) '()]
    [(cons? lors)
     (cons (make-table-row (first lors))
           (make-table (rest lors)))]))

(check-expect (make-table '())
              '())
(check-expect (make-table (ranking ONE-LIST))
              '((tr (td "1") (td "Asia: Heat of the Moment"))
                (tr (td "2") (td "U2: One"))
                (tr (td "3") (td "The White Stripes: Seven Nation Army"))))

; RS -> LRHTR
; Create a list representation of an
; HTML table row.
(define (make-table-row rs)
  `(tr ,(make-table-data (number->string (first rs)))
       ,(make-table-data (second rs))))

(check-expect (make-table-row (list 2 "U2: One"))
              '(tr (td "2") (td "U2: One")))

; String -> LRHTD
; Create a list representation of HTML-table data
; from the string s.
(define (make-table-data s)
  `(td ,s))

(check-expect (make-table-data "2")
              (list 'td "2"))

; LOS -> LORS
; Add ranks to the songs in los, with the
; first song having the highest rank, i.e.
; the lowest number.
(define (ranking los)
  (reverse (add-ranks (reverse los))))

(check-expect (ranking ONE-LIST)
              (list (list 1 "Asia: Heat of the Moment")
                    (list 2 "U2: One")
                    (list 3 "The White Stripes: Seven Nation Army")))

; LOS -> LORS
; Add ranks to the songs in los, with the
; first song having the lowest rank, i.e.
; the highest number.
(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else (cons (list (length los) (first los))
                (add-ranks (rest los)))]))

(check-expect (add-ranks '())
              '())
(check-expect (add-ranks ONE-LIST)
              (list (list 3 "Asia: Heat of the Moment")
                    (list 2 "U2: One")
                    (list 1 "The White Stripes: Seven Nation Army")))
