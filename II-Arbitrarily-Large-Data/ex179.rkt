;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex179) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; Editor KeyEvent -> Editor
; deals with a key event, given some editor
(define (editor-kh ed k)
  (cond
    [(key=? k "left") (editor-lft ed)]
    [(key=? k "right") (editor-rgt ed)]
    [(key=? k "\b") (editor-del ed)]
    [(key=? k "\t") ed]
    [(key=? k "\r") ed]
    [(= (string-length k) 1) (editor-ins ed k)]
    [else ed]))

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

; Editor -> Editor
; moves the cursor position one 1String left, 
; if possible 
(define (editor-lft ed)
  (if (> (length (editor-pre ed)) 0)
      (make-editor (rest (editor-pre ed))
                   (cons (first (editor-pre ed))
                         (editor-post ed)))
      ed))

(check-expect (editor-lft (create-editor "" ""))
              (create-editor "" ""))
(check-expect (editor-lft (create-editor "" "cdfgh"))
              (create-editor "" "cdfgh"))
(check-expect (editor-lft (create-editor "cdfgh" ""))
              (create-editor "cdfg" "h"))
(check-expect (editor-lft (create-editor "cd" "fgh"))
              (create-editor "c" "dfgh"))
 
; Editor -> Editor
; moves the cursor position one 1String right, 
; if possible 
(define (editor-rgt ed)
  (if (> (length (editor-post ed)) 0)
      (make-editor (cons (first (editor-post ed))
                         (editor-pre ed))
                   (rest (editor-post ed)))
      ed))

(check-expect (editor-rgt (create-editor "" ""))
              (create-editor "" ""))
(check-expect (editor-rgt (create-editor "" "cdfgh"))
              (create-editor "c" "dfgh"))
(check-expect (editor-rgt (create-editor "cdfgh" ""))
              (create-editor "cdfgh" ""))
(check-expect (editor-rgt (create-editor "cd" "fgh"))
              (create-editor "cdf" "gh"))
 
; Editor -> Editor
; deletes a 1String to the left of the cursor,
; if possible 
(define (editor-del ed)
  (if (> (length (editor-pre ed)) 0)
      (make-editor (rest (editor-pre ed))
                   (editor-post ed))
      ed))

(check-expect (editor-del (create-editor "" ""))
              (create-editor "" ""))
(check-expect (editor-del (create-editor "" "cdfgh"))
              (create-editor "" "cdfgh"))
(check-expect (editor-del (create-editor "cdfgh" ""))
              (create-editor "cdfg" ""))
(check-expect (editor-del (create-editor "cd" "fgh"))
              (create-editor "c" "fgh"))

; Editor 1String -> Editor
; insert the 1String k between pre and post
(define (editor-ins ed k)
  (make-editor (cons k (editor-pre ed))
               (editor-post ed)))

(check-expect (editor-ins (make-editor '() '()) "e")
              (make-editor (cons "e" '()) '()))
(check-expect (editor-ins (make-editor (cons "d" '())
                                       (cons "f" (cons "g" '())))
                          "e")
              (make-editor (cons "e" (cons "d" '()))
                           (cons "f" (cons "g" '()))))

; from ex172.rkt (exercise 172) – – – – – – – – – – – – – – – – – – – – – – – –
; uses reverse instead of rev

; String String -> Editor
; Create an editor from s1 and s2.
(define (create-editor s1 s2)
  (make-editor (reverse (explode s1))
               (explode s2)))