;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex307) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; String [List-of String] -> Boolean
; Are any of the names on los equal to
; or an extension of name.
; Case insensitive.
(define (find-name name los)
  (local ((define name-lower (string-downcase name))
          (define name-length (string-length name)))
    (for/or ([s los])
      (or (string=? (string-downcase s)
                    name-lower)
          (string=? (string-downcase (substring s 0 name-length))
                    name-lower)))))

(check-expect (find-name "Anna" '())
              #false)

(check-expect (find-name "Anna"
                         (list "Sophie"
                               "Anne"
                               "Charlotte"
                               "Annette"
                               "Christina"))
              #false)
(check-expect (find-name "Anna"
                         (list "Sophie"
                               "Anne"
                               "Charlotte"
                               "Annette"
                               "Christina"
                               "Annalena"
                               "Johanna"))
              #true)

; 1String [List-of String] -> Boolean
; Do all names on los start with ltr?
; Case insensitive.
(define (all-start-with? ltr los)
  (local ((define ltr-lowercase (string-downcase ltr)))
    (for/and ([s los])
      (if (= (string-length s) 0)
          #false
          (string=? ltr-lowercase (string-downcase (substring s 0 1)))))))

(check-expect (all-start-with? "a" '())
              #true)
(check-expect (all-start-with? "a"
                               (list "Anne"
                                     "anna"
                                     "Andrea"
                                     "Angelika"
                                     "Anastasija"
                                     "Annette"
                                     ""))
              #false)
(check-expect (all-start-with? "a"
                               (list "Sophie"
                                     "Anne"
                                     "Charlotte"
                                     "Annette"
                                     "Christina"
                                     "Annalena"
                                     "Johanna"))
              #false)
(check-expect (all-start-with? "a"
                               (list "Anne"
                                     "anna"
                                     "Andrea"
                                     "Angelika"
                                     "Anastasija"
                                     "Annette"))
              #true)
(check-expect (all-start-with? "A"
                               (list "Anne"
                                     "anna"
                                     "Andrea"
                                     "Angelika"
                                     "Anastasija"
                                     "Annette"))
              #true)
(check-expect (all-start-with? "a"
                               (list "Anne"
                                     "anna"
                                     "Andrea"
                                     "Angelika"
                                     "Anastasija"
                                     "Annette"
                                     "Beate"))
              #false)

; [List-of String] N -> Boolean
; Do none of the names on los exceed
; the width n?
(define (length-does-not-exceed-n? los n)
  (for/and ([s los])
    (<= (string-length s) n)))

(check-expect (length-does-not-exceed-n? '() 1)
              #true)
(check-expect (length-does-not-exceed-n?
               (list "Sophie"
                     "Anne"
                     "Charlotte"
                     "Annette"
                     "Christina"
                     "Annalena"
                     "Johanna")
               9)
              #true)
(check-expect (length-does-not-exceed-n?
               (list "Sophie"
                     "Anne"
                     "Charlotte"
                     "Annette"
                     "Christina"
                     "Anastasija"
                     "Annalena"
                     "Johanna")
               9)
              #false)