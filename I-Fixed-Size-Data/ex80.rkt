;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex80) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct movie [title director year])

; Movie -> ???
; Process a Movie structure.
(define (process-move m)
  (... (movie title m) ...
   ... (movie-director m) ...
   ... (movie-year m) ...))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct pet [name number])

; Pet -> ???
; Process a Pet structure.
(define (process-pet p)
  (... (pet-name p) ...
   ... (pet-number p) ...))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct CD [artist title price])

; CD -> ???
; Process a CD structure.
(define (process-CD cd)
  (... (cd-artist cd) ...
   ... (cd-title cd) ...
   ... (cd-price cd) ...))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct sweater [material size color])

; Sweater -> ???
; Process a Sweater structure.
(define (process-sweater s)
  (... (sweater-material s) ...
   ... (sweater-size s) ...
   ... (sweater-color s) ...))