;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex301) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (insertion-sort alon)
  ; alon SCOPE -----------------------------------------------------------------
  ; the whole local is the sort SCOPE ++++++++++++++++++++++++++++++++++++++++++
  (local ((define (sort alon)
            ; alon SCOPE of sort function ######################################
            (cond
              [(empty? alon) '()]
              [else
               (add (first alon) (sort (rest alon)))]))
            ; END alon SCOPE of sort function ##################################
          (define (add an alon)
            ; alon SCOPE add function ::::::::::::::::::::::::::::::::::::::::::
            (cond
              [(empty? alon) (list an)]
              [else
               (cond
                 [(> an (first alon)) (cons an alon)]
                 [else (cons (first alon)
                             (add an (rest alon)))])]))
            ; END alon SCOPE add function ::::::::::::::::::::::::::::::::::::::
          )
    (sort alon)))
  ; END sort SCOPE +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ; END alon SCOPE -------------------------------------------------------------


; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


(define (sort alon)
  ; sort function SCOPE µµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµ
  ; alon SCOPE -----------------------------------------------------------------
  ; inner sort function SCOPE @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  (local ((define (sort alon)
            ; alon SCOPE of sort function ######################################
            (cond
              [(empty? alon) '()]
              [else
               (add (first alon) (sort (rest alon)))]))
            ; END alon SCOPE of sort function ##################################
          (define (add an alon)
            ; alon SCOPE add function ::::::::::::::::::::::::::::::::::::::::::
            (cond
              [(empty? alon) (list an)]
              [else
                (cond
                  [(> an (first alon)) (cons an alon)]
                  [else (cons (first alon)
                              (add an (rest alon)))])]))
            ; END alon SCOPE add function ::::::::::::::::::::::::::::::::::::::
          )
    (sort alon)))
  ; END inner sort function SCOPE @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ; END alon SCOPE -------------------------------------------------------------
  ; END sort function SCOPE µµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµµ