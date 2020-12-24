;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex271) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; A NatNumber (natural number) is one of:
;     – 0
;     – (add1 NatNumber)
; Interpretation:
;     The natural numbers.

; A [List-of ITEM] is one of:
;     – '()
;     – (cons ITEM [List-of ITEM])
; Interpretation:
;     A list of ITEMs.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; String [List-of String] -> Boolean
; Are any of the names on los equal to
; or an extension of name.
; Case insensitive.
(define (find-name name los)
  (local ((define name-lower (string-downcase name))
          (define name-length (string-length name))
          
          ; String -> Boolean
          ; Is str equal to or
          ; an extension of name?
          (define (same-or-ext? str)
            (or (string=? (string-downcase str)
                          name-lower)
                (string=? (string-downcase (substring str 0 name-length))
                          name-lower))))
    
    ; – IN –
    (ormap same-or-ext? los)))

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
  (local ((define ltr-lower (string-downcase ltr))
          
          ; String -> Boolean
          ; Does str start with ltr?
          (define (starts-with? str)
            (string=? (string-downcase (substring str 0 1))
                      ltr-lower)))

    ; – IN –
    (andmap starts-with? los)))

(check-expect (all-start-with? "a" '())
              #true)
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
(check-expect (all-start-with? "a"
                               (list "Anne"
                                     "anna"
                                     "Andrea"
                                     "Angelika"
                                     "Anastasija"
                                     "Annette"
                                     "Beate"))
              #false)

; [List-of String] NatNumber -> Boolean
; Do none of the names on los exceed
; the width n?
(define (length-does-not-exceed-n? los n)
  (local (; String -> Boolean
          ; Is str's length less than or
          ; equal to n?
          (define (shorter-or-as-long-as-n? str)
            (<= (string-length str) n)))

    ; – IN –
    (andmap shorter-or-as-long-as-n? los)))

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