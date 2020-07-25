;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex114_3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct editor [text index])
; An Editor is a structure:
;   (make-editor String Number)
; interpretation (make-editor t i) describes an editor
; whose visible text is t with 
; the cursor displayed at index i

; A KeyEvent is one of: 
; – 1String
; – "left"
; – "right"
; – "up"
; – ...

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define WIDTH 200)
(define HEIGHT 20)
(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

(define TEXT-SIZE (* 17/200 WIDTH))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; String -> Image
; Run an interactive editor.
(define (run s)
  (big-bang (make-editor s (string-length s))
    [on-draw render]
    [on-key edit]
    [check-with editor?]))

; Editor -> Image
; Produce an image representation of the given editor.
(define (render e)
  (overlay/align "left"
                 "bottom"
                 (beside/align "bottom"
                               (text (substring
                                      (editor-text e)
                                      0
                                      (editor-index e))
                                     TEXT-SIZE "black")
                               CURSOR
                               (text (substring
                                      (editor-text e)
                                      (editor-index e))
                                      TEXT-SIZE
                                      "black"))
                 BACKGROUND))

(check-expect (render (make-editor "" 0))
              (overlay/align "left"
                             "bottom"
                             (text "" TEXT-SIZE "black")
                             CURSOR
                             BACKGROUND))
(check-expect (render (make-editor "HELLO WORLD" 6))
              (overlay/align "left"
                             "bottom"
                             (beside/align "bottom"
                                           (text "HELLO " TEXT-SIZE "black")
                                           CURSOR
                                           (text "WORLD" TEXT-SIZE "black"))
                             BACKGROUND))

; Editor KeyEvent -> Editor
; Add ke to the end of the pre-field of e.
(define (edit e ke)
  (cond
    [(> (string-length ke) 1)
     (cond
       [(key=? ke "left")
        (make-editor (editor-text e)
                     (if (>= (sub1 (editor-index e)) 0)
                         (sub1 (editor-index e))
                         0))]
       [(key=? ke "right")
        (make-editor (editor-text e)
                     (if (<= (add1 (editor-index e))
                             (string-length (editor-text e)))
                         (add1 (editor-index e))
                         (editor-index e)))]
       [else e])]
     [(= (string-length ke) 1)
      (cond
        [(key=? ke "\b")
         (if (> (editor-index e) 0)
             (make-editor (string-append
                           (substring (editor-text e) 0 (sub1 (editor-index e)))
                           (substring (editor-text e) (editor-index e)))
                          (sub1 (editor-index e)))
             e)]
        [(key=? ke "\t") e]
        [(key=? ke "\r") e]
        [(key=? ke "\u007F") e]
        [else
         (if (fits? (insert-letter e ke))
             (insert-letter e ke)
             e)])]))
       

(check-expect (edit (make-editor "hello world" 0) "a")
              (make-editor "ahello world" 1))
(check-expect (edit (make-editor "hello world" 6) "a")
              (make-editor "hello aworld" 7))
(check-expect (edit (make-editor "hello world" 11) "a")
              (make-editor "hello worlda" 12))
(check-expect (edit (make-editor "" 0) "a")
              (make-editor "a" 1))

(check-expect (edit (make-editor "hello world" 0) "\b")
              (make-editor "hello world" 0))
(check-expect (edit (make-editor "hello world" 6) "\b")
              (make-editor "helloworld" 5))
(check-expect (edit (make-editor "hello world" 11) "\b")
              (make-editor "hello worl" 10))
(check-expect (edit (make-editor "" 0) "\b")
              (make-editor "" 0))

(check-expect (edit (make-editor "hello world" 6) "\t")
              (make-editor "hello world" 6))
(check-expect (edit (make-editor "hello world" 6) "\r")
              (make-editor "hello world" 6))

; delete key, has length 1
(check-expect (edit (make-editor "hello world" 6) "\u007F")
              (make-editor "hello world" 6))

(check-expect (edit (make-editor "hello world" 0) "left")
              (make-editor "hello world" 0))
(check-expect (edit (make-editor "hello world" 6) "left")
              (make-editor "hello world" 5))
(check-expect (edit (make-editor "hello world" 11) "left")
              (make-editor "hello world" 10))
(check-expect (edit (make-editor "" 0) "left")
              (make-editor "" 0))

(check-expect (edit (make-editor "hello world" 0) "right")
              (make-editor "hello world" 1))
(check-expect (edit (make-editor "hello world" 6) "right")
              (make-editor "hello world" 7))
(check-expect (edit (make-editor "hello world" 11) "right")
              (make-editor "hello world" 11))
(check-expect (edit (make-editor "" 0) "right")
              (make-editor "" 0))

(check-expect (edit (make-editor "hello world" 0) "clear")
              (make-editor "hello world" 0))

(check-expect (edit (make-editor (make-string 22 #\o) 22) "o")
              (make-editor (make-string 22 #\o) 22))

; Editor 1String -> Editor
; Insert s at the cursor position in e.
(define (insert-letter e s)
  (make-editor
   (string-append
    (substring (editor-text e) 0 (editor-index e))
    s
    (substring (editor-text e) (editor-index e)))
   (add1 (editor-index e))))

(check-expect (insert-letter (make-editor "helloworld" 5) " ")
              (make-editor "hello world" 6))

; Editor -> Boolean
; Check whether the text representation of e
; fits into background
(define (fits? e)
  (if (<= (+ (image-width (text (editor-text e)
                                TEXT-SIZE
                                "black"))
             (image-width CURSOR))
          (image-width BACKGROUND))
      #true
      #false))

(check-expect (fits? (make-editor "" 0))
              #true)
(check-expect (fits? (make-editor (make-string 23 #\o) 23))
              #false)