;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex207) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/itunes)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; An LLists is one of:
;; – '()
;; – (cons LAssoc LLists)
; 
;; An LAssoc is one of: 
;; – '()
;; – (cons Association LAssoc)
;; 
;; An Association is a list of two items: 
;;   (cons String (cons BSDN '()))
; 
;; A BSDN is one of: 
;; – Boolean
;; – Number
;; – String
;; – Date
; 
;; String -> LLists
;; creates a list of lists representation for all tracks in 
;; file-name, which must be an XML export from iTunes 
;(define (read-itunes-as-lists file-name)
;  ...)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; modify the following to use your chosen name
(define ITUNES-LOCATION "itunes.xml")
 
; LLists
(define list-tracks
  (read-itunes-as-lists ITUNES-LOCATION))

; DATA EXAMPLES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; LAssoc examples
; these are some of the tracks from list-tracks

(define LASSOC1
  (list
   (list "Track ID" 444)
   (list "Name" "Only Time")
   (list "Artist" "Enya")
   (list "Album" "A Day Without Rain")
   (list "Genre" "New Age")
   (list "Kind" "MPEG audio file")
   (list "Size" 4364035)
   (list "Total Time" 218096)
   (list "Track Number" 3)
   (list "Track Count" 11)
   (list "Year" 2000)
   (list "Date Modified" (create-date 2002 7 17 0 0 21))
   (list "Date Added" (create-date 2002 7 17 3 55 42))
   (list "Bit Rate" 160)
   (list "Sample Rate" 44100)
   (list "Play Count" 18)
   (list "Play Date" 3388484327)
   (list "Play Date UTC" (create-date 2011 5 17 17 38 47))
   (list "Sort Album" "Day Without Rain")
   (list "Persistent ID" "EBBE9171392FA34A")
   (list "Track Type" "File")
   (list
    "Location"
    (string-append "file://localhost/Users/matthias/Music/iTunes/"
                   "iTunes%20Music/Enya/A%20Day%20Without%20Rain/"
                   "03%20Only%20Time.mp3"))
   (list "File Folder Count" 4)
   (list "Library Folder Count" 1)))

(define LASSOC2
  (list
   (list "Track ID" 1728)
   (list "Name" "Be Mine")
   (list "Artist" "R.E.M.")
   (list "Composer" "Berry, Bill/Michael Stipe/Mike Mills/Peter Buck")
   (list "Album" "New Adventures In Hi-Fi")
   (list "Genre" "Rock")
   (list "Kind" "MPEG audio file")
   (list "Size" 6663463)
   (list "Total Time" 333061)
   (list "Disc Number" 1)
   (list "Disc Count" 1)
   (list "Track Number" 9)
   (list "Track Count" 14)
   (list "Year" 1996)
   (list "Date Modified" (create-date 2004 2 11 10 11 53))
   (list "Date Added" (create-date 2004 2 11 15 11 32))
   (list "Bit Rate" 160)
   (list "Sample Rate" 44100)
   (list "Play Count" 14)
   (list "Play Date" 3414756546)
   (list "Play Date UTC" (create-date 2012 3 16 19 29 6))
   (list "Normalization" 5850)
   (list "Persistent ID" "3C60666B96C087D6")
   (list "Track Type" "File")
   (list
    "Location"
    (string-append "file://localhost/Users/matthias/Music/iTunes/"
                   "iTunes%20Music/R.E.M_/New%20Adventures%20In%20Hi-Fi/"
                   "09%20Be%20Mine.mp3"))
   (list "File Folder Count" 4)
   (list "Library Folder Count" 1)))

(define LASSOC3
  (list
   (list "Track ID" 3990)
   (list "Name" "He Can Only Hold Her")
   (list "Artist" "Amy Winehouse")
   (list "Album Artist" "Amy Winehouse")
   (list "Composer"
         (string-append "Amy Winehouse, Richard Poindexter, "
                        "Robert Poindexter 38 John Harrison"))
   (list "Album" "iTunes Festival: London 2007")
   (list "Genre" "Pop")
   (list "Kind" "Purchased AAC audio file")
   (list "Size" 6856755)
   (list "Total Time" 191866)
   (list "Disc Number" 1)
   (list "Disc Count" 1)
   (list "Track Number" 7)
   (list "Track Count" 8)
   (list "Year" 2007)
   (list "Date Modified" (create-date 2014 10 11 23 43 14))
   (list "Date Added" (create-date 2014 10 20 14 31 25))
   (list "Bit Rate" 256)
   (list "Sample Rate" 44100)
   (list "Play Count" 6)
   (list "Play Date" 3498740411)
   (list "Play Date UTC" (create-date 2014 11 13 21 20 11))
   (list "Release Date" (create-date 2007 8 13 7 0 0))
   (list "Normalization" 5811)
   (list "Artwork Count" 1)
   (list "Persistent ID" "909150FDAC57DBBF")
   (list "Track Type" "File")
   (list "Purchased" #true)
   (list
    "Location"
    (string-append "file://localhost/Users/matthias/Music/iTunes/"
                   "iTunes%20Media/Music/Amy%20Winehouse/"
                   "iTunes%20Festival_%20London%202007/"
                   "07%20He%20Can%20Only%20Hold%20Her.m4a"))
   (list "File Folder Count" 5)
   (list "Library Folder Count" 1)))

(define LASSOC4
  (list
   (list "Track ID" 3992)
   (list "Name" "Monkey Man")
   (list "Artist" "Amy Winehouse")
   (list "Album Artist" "Amy Winehouse")
   (list "Composer" "Frederick Hibbert")
   (list "Album" "iTunes Festival: London 2007")
   (list "Genre" "Pop")
   (list "Kind" "Purchased AAC audio file")
   (list "Size" 6769836)
   (list "Total Time" 188813)
   (list "Disc Number" 1)
   (list "Disc Count" 1)
   (list "Track Number" 8)
   (list "Track Count" 8)
   (list "Year" 2007)
   (list "Date Modified" (create-date 2014 10 11 23 43 10))
   (list "Date Added" (create-date 2014 10 20 14 31 25))
   (list "Bit Rate" 256)
   (list "Sample Rate" 44100)
   (list "Play Count" 6)
   (list "Play Date" 3498740599)
   (list "Play Date UTC" (create-date 2014 11 13 21 23 19))
   (list "Release Date" (create-date 2007 8 13 7 0 0))
   (list "Normalization" 4904)
   (list "Artwork Count" 1)
   (list "Persistent ID" "39C9BD4120CCF2C2")
   (list "Track Type" "File")
   (list "Purchased" #true)
   (list
    "Location"
    (string-append "file://localhost/Users/matthias/Music/iTunes/"
                   "iTunes%20Media/Music/Amy%20Winehouse/"
                   "iTunes%20Festival_%20London%202007/08%20Monkey%20Man.m4a"))
   (list "File Folder Count" 5)
   (list "Library Folder Count" 1)))

; LLists example
(define LLISTS1 (list LASSOC1
                      LASSOC2
                      LASSOC3
                      LASSOC4))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; LLists -> Number
; Return the total play time of all tracks
; in ll in milliseconds.
(define (total-time/list ll)
  (cond
    [(empty? ll) 0]
    [(cons? ll)
     (+ (second (find-association "Total Time" (first ll) 0))
        (total-time/list (rest ll)))]))

(check-expect (total-time/list '())
              0)
(check-expect (total-time/list LLISTS1)
              (+ (second (find-association "Total Time" LASSOC1 0))
                 (second (find-association "Total Time" LASSOC2 0))
                 (second (find-association "Total Time" LASSOC3 0))
                 (second (find-association "Total Time" LASSOC4 0))))

; String Lassoc Any -> Association or Any
; Produce the first Association whose first item is equal to key,
; or default if there is no such Association.
(define (find-association key lassoc default)
  (cond
    [(empty? lassoc) default]
    [(cons? lassoc)
     (if (string=? (first (first lassoc)) key)
         (first lassoc)
         (find-association key (rest lassoc) default))]))

(check-expect (find-association "Date Modified" LASSOC4 #false)
              (list "Date Modified" (create-date 2014 10 11 23 43 10)))
(check-expect (find-association "No Such Thing" LASSOC4 #false)
              #false)

; from exercise 200 (ex200.rkt) ------------------------------------------------
 
; LTracks
(define itunes-tracks
  (read-itunes-as-tracks ITUNES-LOCATION))

; LTracks -> Number
; Produce the total amount of play time in milliseconds.
(define (total-time lt)
  (cond
    [(empty? lt) 0]
    [(cons? lt)
     (+ (track-time (first lt))
        (total-time (rest lt)))]))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Once you have completed the design, compute the total play time of your iTunes
; collection. Compare this result with the time that the total-time function
; from exercise 200 computes. Why is there a difference?

(total-time/list list-tracks)
(total-time itunes-tracks)

(length list-tracks)
(length itunes-tracks)

; From the introduction to 12.2 Real-World Data: iTunes ------------------------

; As always, the first task is to choose a BSL data representation for this
; information. In this section, we use two representations for music tracks:
; a structure-based one and another based on lists. While the former records a
; fixed number of attributes per track and only if all information is available,
; the latter comes with whatever information is available represented as data.
; Each serves particular uses well; for some uses, both representations are
; useful.

; ------------------------------------------------------------------------------

; Since the function representing tracks as lists is not so "picky" about the
; available information, it has more elements than the representation based on
; structures. Therefore its total playing time is longer.