;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex188) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; NOTE - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; The sort and insert functions are repetitive. One alternative would be to
; pass them another argument, the comparison function / predicate. Thus
; one could use the same function and just change one argument depending
; on the task to be accomplished.

; END NOTE - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct email [from date message])
; An Email-Message is a structure: 
;   (make-email String Number String)
; interpretation (make-email f d m) represents text m 
; sent by f, d seconds after the beginning of time

; NOTE - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; I interpret "seconds afte the beginning of time" as Unix time, i.e.
; seconds since 00:00:00 UTC on 1 January 1970.

; Links
; [1] https://en.wikipedia.org/wiki/Unix_time (retrieved 15.08.2020)
; to generate random dates + times and convert them to Unix time
; [2] https://www.random.org/calendar-dates/ (retrieved 15.08.2020)
; [3] https://www.random.org/clock-times/ (retrieved 15.08.2020)
; [4] https://www.unixtimestamp.com/ (retrieved 15.08.2020)

; alternative to [4]
; https://www.epochconverter.com/

; END NOTE - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; A List-of-Emails is one of:
; – '()
; – (cons Email-Message List-of-Emails)
; Interpretation:
;     A list containing Email-Messages.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; 1977-08-04 05:30
(define MAIL1 (make-email "Abradolf Linkler"
                          239520600
                          "Peace and Unity to the World."))
; 1981-08-06 08:11
(define MAIL2 (make-email "Rick Sanchez"
                          365933460
                          "There is not God."))
; 1994-07-17 09:34
(define MAIL3 (make-email "Morty Smith"
                          774437640
                          "Come watch TV?"))
; 2009-11-13 14:49
(define MAIL4 (make-email "Summer Smith"
                          1258104840
                          "Oh my god."))
; 2014-04-05 22:38
(define MAIL5 (make-email "Beth Smith"
                          1396737480
                          "Am I the real Beth?"))
; 2009-11-13 17:33
(define MAIL6 (make-email "Jerry Smith"
                          1258133580
                          "Hello? Is anybody here?"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; List-of-Emails -> List-of-Emails
; Sort loe by date (descending order).
(define (sort-by-date loe)
  (cond
    [(empty? loe) '()]
    [(cons? loe)
     (insert-email-time (first loe) (sort-by-date (rest loe)))]))

(check-expect (sort-by-date '())
              '())
(check-expect (sort-by-date (list MAIL1))
              (list MAIL1))
(check-expect (sort-by-date (list MAIL5 MAIL6 MAIL4 MAIL3 MAIL2 MAIL1))
              (list MAIL5 MAIL6 MAIL4 MAIL3 MAIL2 MAIL1))
(check-expect (sort-by-date (list MAIL1 MAIL2 MAIL3 MAIL4 MAIL6 MAIL5))
              (list MAIL5 MAIL6 MAIL4 MAIL3 MAIL2 MAIL1))
(check-expect (sort-by-date (list MAIL1 MAIL6 MAIL5 MAIL2 MAIL4 MAIL3))
              (list MAIL5 MAIL6 MAIL4 MAIL3 MAIL2 MAIL1))

; List-of-Emails -> List-of-Emails
; Sort loe alphabetically by name.
(define (sort-by-name loe)
  (cond
    [(empty? loe) '()]
    [(cons? loe)
     (insert-email-name (first loe) (sort-by-name (rest loe)))]))

(check-expect (sort-by-name '())
              '())
(check-expect (sort-by-name (list MAIL5))
              (list MAIL5))
(check-expect (sort-by-name (list MAIL1 MAIL5 MAIL6 MAIL3 MAIL2 MAIL4))
              (list MAIL1 MAIL5 MAIL6 MAIL3 MAIL2 MAIL4))
(check-expect (sort-by-name (list MAIL4 MAIL2 MAIL3 MAIL6 MAIL5 MAIL1))
              (list MAIL1 MAIL5 MAIL6 MAIL3 MAIL2 MAIL4))
(check-expect (sort-by-name (list MAIL6 MAIL1 MAIL2 MAIL3 MAIL3 MAIL5 MAIL4))
              (list MAIL1 MAIL5 MAIL6 MAIL3 MAIL3 MAIL2 MAIL4))

; Email-Message List-of-Emails -> List-of-Emails
; Insert e in the sorted list loe (by date, descending).
(define (insert-email-time e loe)
  (cond
    [(empty? loe) (list e)]
    [(cons? loe)
     (if (email-date>=? e (first loe))
         (cons e loe)
         (cons (first loe) (insert-email-time e (rest loe))))]))

(check-expect (insert-email-time MAIL1 '())
              (list MAIL1))
(check-expect (insert-email-time MAIL6 (list MAIL5 MAIL4 MAIL3 MAIL2 MAIL1))
              (list MAIL5 MAIL6 MAIL4 MAIL3 MAIL2 MAIL1))

; Email-Message List-of-Emails -> List-of-Emails
; Insert e in the sorted list loe (alpbetically, by name).
(define (insert-email-name e loe)
  (cond
    [(empty? loe) (list e)]
    [(cons? loe)
     (if (string<? (email-from e) (email-from (first loe)))
         (cons e loe)
         (cons (first loe) (insert-email-name e (rest loe))))]))

(check-expect (insert-email-name MAIL3 '())
              (list MAIL3))
(check-expect (insert-email-name MAIL3
                                 (list MAIL1 MAIL5 MAIL6 MAIL3 MAIL2 MAIL4))
              (list MAIL1 MAIL5 MAIL6 MAIL3 MAIL3 MAIL2 MAIL4))

; Email-Message Email-Message -> Boolean
; Is the date of e1 greater or equal to the date of e2,
; i.e. was e1 sent later than e2 (or at the same time)?
(define (email-date>=? e1 e2)
  (>= (email-date e1) (email-date e2)))

(check-expect (email-date>=? MAIL1 MAIL1)
              #true)
(check-expect (email-date>=? MAIL1 MAIL2)
              #false)
(check-expect (email-date>=? MAIL4 MAIL3)
              #true)