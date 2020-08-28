;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex208) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/itunes)
(require racket/list)    ; for flatten function

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

; A List-of-Strings is one of:
; – '()
; – (cons String List-of-Strings)
; Interpretation:
;     A list containing Strings.

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

; LLists -> List-of-Strings
; Produce the Strings that are
; associated with a Boolean attribute.
(define (boolean-attributes ll)
  (cond
    [(empty? ll) '()]
    [(cons? ll)
     (create-set
      (flatten
       (cons
        (select-boolean-attributes (first ll))
        (boolean-attributes (rest ll)))))]))

(check-expect (boolean-attributes '())
              '())
(check-expect (boolean-attributes LLISTS1)
              (list "Purchased"))

; LAssoc -> List-of-Strings
; Produce the Strings that are associated
; with a Boolean Attribute for the track lassoc.
(define (select-boolean-attributes lassoc)
  (cond
    [(empty? lassoc) '()]
    [(cons? lassoc)
     (if (boolean? (second (first lassoc)))
         (cons (first (first lassoc)) (select-boolean-attributes (rest lassoc)))
         (select-boolean-attributes (rest lassoc)))]))

(check-expect (select-boolean-attributes '())
              '())
(check-expect (select-boolean-attributes LASSOC1)
              '())
(check-expect (select-boolean-attributes LASSOC4)
              (list "Purchased"))

; from exercise 201 (ex201.rkt) ------------------------------------------------

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

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(boolean-attributes list-tracks)