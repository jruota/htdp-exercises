;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex276) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/itunes)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; An LTracks is one of:
;; – '()
;; – (cons Track LTracks)
; 
;(define-struct track
;  [name artist album time track# added play# played])
;; A Track is a structure:
;;   (make-track String String String N N Date N Date)
;; interpretation An instance records in order: the track's 
;; title, its producing artist, to which album it belongs, 
;; its playing time in milliseconds, its position within the 
;; album, the date it was added, how often it has been 
;; played, and the date when it was last played
; 
;(define-struct date [year month day hour minute second])
;; A Date is a structure:
;;   (make-date N N N N N N)
;; interpretation An instance records six pieces of information:
;; the date's year, month (between 1 and 12 inclusive), 
;; day (between 1 and 31), hour (between 0 
;; and 23), minute (between 0 and 59), and 
;; second (also between 0 and 59).

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; modify the following to use your chosen name
;(define ITUNES-LOCATION "itunes.xml")
; 
;; LTracks
;(define itunes-tracks
;  (read-itunes-as-tracks ITUNES-LOCATION))

; DATA EXAMPLES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Date examples
(define DATE1 (create-date 2020 08 22 07 54 25))
(define DATE2 (create-date 2014 10 20 14 31 03))
(define DATE3 (create-date 1998 04 05 20 34 47))

; Track examples
; these are the last three tracks of itunes-tracks plus the first one
(define TRACK1 (create-track
                "You Know I'm No Good"
                "Amy Winehouse"
                "iTunes Festival: London 2007"
                263613
                6
                (create-date 2014 10 20 14 31 25)
                6
                (create-date 2014 11 13 21 16 59)))

(define TRACK2 (create-track
                "He Can Only Hold Her"
                "Amy Winehouse"
                "iTunes Festival: London 2007"
                191866
                7
                (create-date 2014 10 20 14 31 25)
                6
                (create-date 2014 11 13 21 20 11)))

(define TRACK3 (create-track
                "Monkey Man"
                "Amy Winehouse"
                "iTunes Festival: London 2007"
                188813
                8
                (create-date 2014 10 20 14 31 25)
                6
                (create-date 2014 11 13 21 23 19)))

(define TRACK4 (create-track
                "Wild Child"
                "Enya"
                "A Day Without Rain"
                227996
                2
                (create-date 2002 7 17 3 55 14)
                20
                (create-date 2011 5 17 17 35 13)))

; LTracks example
(define LTRACKS1 (list TRACK1 TRACK2 TRACK3 TRACK4))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; LTracks -> List-of-LTracks
; Produce a list of LTracks, one per album.
(define (select-albums lt)
  (tracks-by-album (select-album-titles/unique lt) lt))

(check-expect (select-albums '())
              '())
(check-expect (select-albums LTRACKS1)
              (list (list TRACK1
                          TRACK2
                          TRACK3)
                    (list TRACK4)))

; [List-of String] LTracks -> List-of-LTracks
; Produce a list of LTracks, one per
; album in los.
(define (tracks-by-album los lt)
  (map (lambda (al) (select-album al lt)) los))

(check-expect (tracks-by-album '() '())
              '())
(check-expect (tracks-by-album '() LTRACKS1)
              '())
(check-expect (tracks-by-album (list "iTunes Festival: London 2007")
                               '())
              (list '()))
(check-expect (tracks-by-album (list "There Is No Album")
                               LTRACKS1)
              (list '()))
(check-expect (tracks-by-album (list "iTunes Festival: London 2007")
                               LTRACKS1)
              (list (list TRACK1 TRACK2 TRACK3)))
(check-expect (tracks-by-album (list "iTunes Festival: London 2007"
                                     "A Day Without Rain")
                               LTRACKS1)
              (list (list TRACK1 TRACK2 TRACK3)
                    (list TRACK4)))

; LTracks -> List-of-Strings
; Produce a list of unique album titles in lt. 
(define (select-album-titles/unique lt)
  (create-set (select-all-album-titles lt)))

(check-expect (select-album-titles/unique '())
              '())
(check-expect (select-album-titles/unique LTRACKS1)
              (list "iTunes Festival: London 2007"
                    "A Day Without Rain"))

; LTracks -> List-of-Strings
; Produce the list of all album titles in lt.
(define (select-all-album-titles lt)
  (map
   (lambda (t) (track-album t))
   lt))

(check-expect (select-all-album-titles '())
              '())
(check-expect (select-all-album-titles LTRACKS1)
              (list "iTunes Festival: London 2007"
                    "iTunes Festival: London 2007"
                    "iTunes Festival: London 2007"
                    "A Day Without Rain"))

; List-of-Strings -> List-of-Strings
; Construct a list that contains every String
; from the given list exactly once.
(define (create-set los)
  (cond
    [(empty? los) '()]
    [(cons? los)
     (if (member? (first los) (rest los))
         (create-set (rest los))
         (cons (first los)
               (create-set (rest los))))]))

(check-expect (create-set '())
              '())
(check-expect (create-set (list "iTunes Festival: London 2007"))
              (list "iTunes Festival: London 2007"))
(check-expect (create-set (select-all-album-titles LTRACKS1))
              (list "iTunes Festival: London 2007"
                    "A Day Without Rain"))

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; String Date LTracks -> LTracks
; Extracts from lt the list of tracks
; that belong to the given album al and
; have been played after the given date d.
(define (select-album-date al d lt)
  (filter
   (lambda (t)
     (and (string=? (track-album t) al)
          (date<? d (track-played t))))
   lt))

(check-expect (select-album-date "iTunes Festival: London 2007"
                                 (create-date 2011 09 29 17 33 29)
                                 '())
              '())
(check-expect (select-album-date "iTunes Festival: London 2007"
                                 (create-date 2011 09 29 17 33 29)
                                 LTRACKS1)
              (list TRACK1 TRACK2 TRACK3))
; year
(check-expect (select-album-date "iTunes Festival: London 2007"
                                 (create-date 2015 11 13 21 16 59)
                                 LTRACKS1)
              '())
; month
(check-expect (select-album-date "iTunes Festival: London 2007"
                                 (create-date 2014 12 13 21 16 59)
                                 LTRACKS1)
              '())
; day
(check-expect (select-album-date "iTunes Festival: London 2007"
                                 (create-date 2014 11 14 21 16 59)
                                 LTRACKS1)
              '())
; minutes
(check-expect (select-album-date "iTunes Festival: London 2007"
                                 (create-date 2014 11 13 21 24 19)
                                 LTRACKS1)
              '())
; seconds
(check-expect (select-album-date "iTunes Festival: London 2007"
                                 (create-date 2014 11 13 21 23 20)
                                 LTRACKS1)
              '())

; Date LTracks -> LTracks
; Extract all tracks from lt that have
; been played after the given date d.
(define (played-after d lt)
  (filter
   (lambda (t) (date<? d (track-played t)))
   lt))

(check-expect (played-after (create-date 2011 5 17 17 35 12) '())
              '())
(check-expect (played-after (create-date 2011 5 17 17 35 12) LTRACKS1)
              LTRACKS1)
(check-expect (played-after (create-date 2014 11 13 21 16 59) LTRACKS1)
              (list TRACK2 TRACK3))

; Date Date -> Boolean
; Is d1 before d2?
(define (date<? d1 d2)
  (cond
    [(or (< (date-year d1) (date-year d2))
         (and (= (date-year d1) (date-year d2))
              (< (date-month d1) (date-month d2)))
         (and (= (date-year d1) (date-year d2))
              (= (date-month d1) (date-month d2))
              (< (date-day d1) (date-day d2)))
         (and (= (date-year d1) (date-year d2))
              (= (date-month d1) (date-month d2))
              (= (date-day d1) (date-day d2))
              (< (date-hour d1) (date-hour d2)))
         (and (= (date-year d1) (date-year d2))
              (= (date-month d1) (date-month d2))
              (= (date-day d1) (date-day d2))
              (= (date-hour d1) (date-hour d2))
              (< (date-minute d1) (date-minute d2)))
         (and (= (date-year d1) (date-year d2))
              (= (date-month d1) (date-month d2))
              (= (date-day d1) (date-day d2))
              (= (date-hour d1) (date-hour d2))
              (= (date-minute d1) (date-minute d2))
              (< (date-second d1) (date-second d2))))
     #true]
    [else #false]))

(check-expect (date<? (create-date 2014 11 12 21 23 20)
                      (create-date 2014 11 12 21 23 20))
              #false)
(check-expect (date<? (create-date 2014 11 12 21 23 20)
                      (create-date 2014 11 12 21 23 21))
              #true)
(check-expect (date<? (create-date 2014 11 12 21 23 20)
                      (create-date 2014 11 12 21 24 20))
              #true)
(check-expect (date<? (create-date 2014 11 12 21 23 20)
                      (create-date 2014 11 12 22 23 21))
              #true)
(check-expect (date<? (create-date 2014 11 12 21 23 20)
                      (create-date 2014 11 13 21 23 21))
              #true)
(check-expect (date<? (create-date 2014 11 12 21 23 20)
                      (create-date 2014 12 12 21 23 21))
              #true)
(check-expect (date<? (create-date 2014 11 12 21 23 20)
                      (create-date 2015 11 12 21 23 21))
              #true)

; String LTracks -> LTracks
; extracts from lt the tracks that belong to the given album al.
(define (select-album al lt)
  (filter
   (lambda (t) (string=? (track-album t) al))
   lt))

(check-expect (select-album "iTunes Festival: London 2007" '())
              '())
(check-expect (select-album "iTunes Festival: London 2007" LTRACKS1)
              (list TRACK1 TRACK2 TRACK3))
(check-expect (select-album "A Day Without Rain" LTRACKS1)
              (list TRACK4))
