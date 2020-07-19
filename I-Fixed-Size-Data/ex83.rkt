;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex83) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with 
; the cursor displayed between s and t

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define BACKGROUND (empty-scene 200 20))
(define CURSOR (rectangle 1 20 "solid" "red"))

(define TEXT-SIZE 17)

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Editor -> Image
; Produce an image representation of the given editor.
(define (render e)
  (overlay/align "left"
                 "bottom"
                 (beside/align "bottom"
                               (text (editor-pre e) TEXT-SIZE "black")
                               CURSOR
                               (text (editor-post e) TEXT-SIZE "black"))
                 BACKGROUND))

(check-expect (render (make-editor "" ""))
              (overlay/align "left"
                             "bottom"
                             (text "" TEXT-SIZE "black")
                             CURSOR
                             BACKGROUND))
(check-expect (render (make-editor "HELLO " "WORLD"))
              (overlay/align "left"
                             "bottom"
                             (beside/align "bottom"
                                           (text "HELLO " TEXT-SIZE "black")
                                           CURSOR
                                           (text "WORLD" TEXT-SIZE "black"))
                             BACKGROUND))