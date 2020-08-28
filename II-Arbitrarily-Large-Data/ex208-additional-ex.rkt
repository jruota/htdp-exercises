;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex208-additional-ex) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
;
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

; DATA EXAMPLES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define TRACK1 (create-track
                "Monkey Man"
                "Amy Winehouse"
                "iTunes Festival: London 2007"
                188813
                8
                (create-date 2014 10 20 14 31 25)
                6
                (create-date 2014 11 13 21 23 19)))

(define LASSOC1
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

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; LAssoc -> Track or #false
; Turn the list representation of a track
; into a structure based representation if possible,
; return #false otherwise.
(define (track-as-struct lassoc)
  (cond
    [(empty? lassoc) #false]
    [(cons? lassoc)
     (if (complete? lassoc)
         (create-track (second (find-association "Name" lassoc #false))
                       (second (find-association "Artist" lassoc #false))
                       (second (find-association "Album" lassoc #false))
                       (second (find-association "Total Time" lassoc #false))
                       (second (find-association "Track Number" lassoc #false))
                       (second (find-association "Date Added" lassoc #false))
                       (second (find-association "Play Count" lassoc #false))
                       (second (find-association "Play Date UTC"
                                                 lassoc
                                                 #false)))
         #false)]))     

(check-expect (track-as-struct '())
              #false)
; date added and play date missing
(check-expect (track-as-struct (list
                                (list "Name" "Song")
                                (list "Artist" "Someone")
                                (list "Album" "Best Of")
                                (list "Total Time" 0)
                                (list "Track Number" 1)
                                (list "Play Count" 2)))
              #false)
(check-expect (track-as-struct LASSOC1)
              TRACK1)

; LAssoc -> Boolean
; Does lassoc have all necessary information
; to produce its structure-based representation?
(define (complete? lassoc)
  (and (cons? (find-association "Name" lassoc #false))
       (cons? (find-association "Artist" lassoc #false))
       (cons? (find-association "Album" lassoc #false))
       (cons? (find-association "Total Time" lassoc #false))
       (cons? (find-association "Track Number" lassoc #false))
       (cons? (find-association "Date Added" lassoc #false))
       (cons? (find-association "Play Count" lassoc #false))
       (cons? (find-association "Play Date UTC" lassoc #false))))

(check-expect (complete? '())
              #false)
(check-expect (complete? (list
                          (list "Name" "Song")
                          (list "Artist" "Someone")
                          (list "Album" "Best Of")
                          (list "Total Time" 0)
                          (list "Track Number" 1)
                          (list "Play Count" 2)))
              #false)
(check-expect (complete? LASSOC1)
              #true)
  
; from exercise 206 (ex206.rkt) ------------------------------------------------

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