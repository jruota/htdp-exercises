;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex396) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An HM-Word is a [List-of Letter or "_"].
; Interpretation:
;     "_" represents a letter to be guessed 

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; HM-Word N -> String
; Run a simplistic hangman game, produce the current state.
(define (play the-pick time-limit)
  (local ((define the-word  (explode the-pick))
          (define the-guess (make-list (length the-word) "_"))

          ; HM-Word -> HM-Word
          (define (do-nothing s) s)

          ; HM-Word KeyEvent -> HM-Word
          (define (checked-compare current-status ke)
            (if (member? ke LETTERS)
                (compare-word the-word current-status ke)
                current-status))

          ; HM-Word -> Boolean
          ; End the game when the word has
          ; been guessed correctly.
          (define (stop hmw)
            (not (member? "_" hmw)))

          ; HM-Word -> Image
          ; Render the solution.
          (define (render-solution hmw)
            (text (implode the-word) 22 "red")))

    ; – IN –
    (implode
     (big-bang the-guess ; HM-Word
       [to-draw render-word]
       [on-tick do-nothing 1 time-limit]
       [on-key  checked-compare]
       [stop-when stop render-solution]))))

; HM-Word -> Image
; Render w.
(define (render-word w)
  (text (implode w) 22 "black"))

; [List-of 1String] HM-Word KeyEvent -> HM-Word
; Return an HM-Word with all "_" replaced by ke
; if there is an ke in the word.
(define (compare-word lo1s hmw ke)
  (cond
    [(empty? lo1s) hmw]
    [(cons? lo1s)
     (if (string=? ke (first lo1s))
         (cons ke (compare-word (rest lo1s) (rest hmw) ke))
         (cons (first hmw) (compare-word (rest lo1s) (rest hmw) ke)))]))

(check-expect (compare-word (explode "hangman")
                            (explode "_______")
                            "n")
              (explode "__n___n"))
(check-expect (compare-word (explode "hangman")
                            (explode "_______")
                            "z")
              (explode "_______"))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define LOCATION "/usr/share/dict/words") ; on OS X
(define AS-LIST (read-lines LOCATION))
(define SIZE (length AS-LIST))
(play (list-ref AS-LIST (random SIZE)) 30)