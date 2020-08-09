;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex178) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor Lo1S Lo1S) 
; An Lo1S is one of: 
; – '()
; – (cons 1String Lo1S)

; CONSTANTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define HEIGHT 20) ; the height of the editor 
(define WIDTH 200) ; its width 
(define FONT-SIZE 16) ; the font size 
(define FONT-COLOR "black") ; the font color 
 
(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; main : String -> Editor
; launches the editor given some initial string 
(define (main s)
   (big-bang (create-editor s "")
     [on-key editor-kh]
     [to-draw editor-render]))

; Editor -> Image
; renders an editor as an image of the two texts 
; separated by the cursor 
(define (editor-render e) MT)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

; Explain why the template for editor-kh deals with "\t" and "\r" before it
; checks for strings of length 1.

; While the characters "\t" and "\r" have length one, they are supposed
; to be ignored by the key-event handler, while other keys of length one
; should change the state of the world. With the exception of "\b", these
; keys trigger an insertion, while "\t" and "\r" are to be ignored. They
; therefore have to be "filtered out" before all other keys of length one.

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

; Editor KeyEvent -> Editor
; deals with a key event, given some editor
(define (editor-kh ed k)
  (cond
    [(key=? k "left") ...]
    [(key=? k "right") ...]
    [(key=? k "\b") ...]
    [(key=? k "\t") ...]
    [(key=? k "\r") ...]
    [(= (string-length k) 1) ...]
    [else ...]))

(check-expect (editor-kh (create-editor "" "") "e")
              (create-editor "e" ""))
(check-expect (editor-kh (create-editor "cd" "fgh") "e")
              (create-editor "cde" "fgh"))

(check-expect (editor-kh (create-editor "" "") "\b")
              (create-editor "" ""))
(check-expect (editor-kh (create-editor "" "cdfgh") "\b")
              (create-editor "" "cdfgh"))
(check-expect (editor-kh (create-editor "cdfgh" "") "\b")
              (create-editor "cdfg" ""))
(check-expect (editor-kh (create-editor "cd" "fgh") "\b")
              (create-editor "c" "fgh"))

(check-expect (editor-kh (create-editor "" "") "left")
              (create-editor "" ""))
(check-expect (editor-kh (create-editor "" "cdfgh") "left")
              (create-editor "" "cdfgh"))
(check-expect (editor-kh (create-editor "cdfgh" "") "left")
              (create-editor "cdfg" "h"))
(check-expect (editor-kh (create-editor "cd" "fgh") "left")
              (create-editor "c" "dfgh"))

(check-expect (editor-kh (create-editor "" "") "right")
              (create-editor "" ""))
(check-expect (editor-kh (create-editor "" "cdfgh") "right")
              (create-editor "c" "dfgh"))
(check-expect (editor-kh (create-editor "cdfgh" "") "right")
              (create-editor "cdfgh" ""))
(check-expect (editor-kh (create-editor "cd" "fgh") "right")
              (create-editor "cdf" "gh"))

; from ex172.rkt (exercise 172) – – – – – – – – – – – – – – – – – – – – – – – –
; uses reverse instead of rev

; String String -> Editor
; Create an editor from s1 and s2.
(define (create-editor s1 s2)
  (make-editor (reverse (explode s1))
               (explode s2)))