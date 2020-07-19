;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex86) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with 
; the cursor displayed between s and t

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
  (big-bang (make-editor s "")
    [on-draw render]
    [on-key edit]))

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

; Editor KeyEvent -> Editor
; Add ke to the end of the pre-field of e.
(define (edit e ke)
  (cond
    [(> (string-length ke) 1)
     (cond
       [(key=? ke "left")
        (make-editor (string-remove-last (editor-pre e))
                     (string-append (string-last (editor-pre e))
                                    (editor-post e)))]
       [(key=? ke "right")
        (make-editor (string-append (editor-pre e)
                                    (string-first (editor-post e)))
                     (string-rest (editor-post e)))]
       [else e])]
     [(= (string-length ke) 1)
      (cond
        [(key=? ke "\b")
         (make-editor (string-remove-last (editor-pre e))
                      (editor-post e))]
        [(key=? ke "\t") e]
        [(key=? ke "\r") e]
        [else
         (if (fits? (make-editor (string-append (editor-pre e) ke)
                                 (editor-post e)))
             (make-editor (string-append (editor-pre e) ke)
                          (editor-post e))
             e)])]))
       

(check-expect (edit (make-editor "" "hello world") "a")
              (make-editor "a" "hello world"))
(check-expect (edit (make-editor "hello " "world") "a")
              (make-editor "hello a" "world"))
(check-expect (edit (make-editor "hello world" "") "a")
              (make-editor "hello worlda" ""))
(check-expect (edit (make-editor "" "") "a")
              (make-editor "a" ""))

(check-expect (edit (make-editor "" "hello world") "\b")
              (make-editor "" "hello world"))
(check-expect (edit (make-editor "hello " "world") "\b")
              (make-editor "hello" "world"))
(check-expect (edit (make-editor "hello world" "") "\b")
              (make-editor "hello worl" ""))
(check-expect (edit (make-editor "" "") "\b")
              (make-editor "" ""))

(check-expect (edit (make-editor "" "hello world") "left")
              (make-editor "" "hello world"))
(check-expect (edit (make-editor "hello " "world") "left")
              (make-editor "hello" " world"))
(check-expect (edit (make-editor "hello world" "") "left")
              (make-editor "hello worl" "d"))
(check-expect (edit (make-editor "" "") "left")
              (make-editor "" ""))

(check-expect (edit (make-editor "" "hello world") "right")
              (make-editor "h" "ello world"))
(check-expect (edit (make-editor "hello " "world") "right")
              (make-editor "hello w" "orld"))
(check-expect (edit (make-editor "hello world" "") "right")
              (make-editor "hello world" ""))
(check-expect (edit (make-editor "" "") "right")
              (make-editor "" ""))

(check-expect (edit (make-editor "hello" "world") "\t")
              (make-editor "hello" "world"))
(check-expect (edit (make-editor "hello" "world") "\r")
              (make-editor "hello" "world"))

(check-expect (edit (make-editor (make-string 22 #\o) "") "o")
              (make-editor (make-string 22 #\o) ""))

; String -> String
; Return s without its last letter if it is non-empty,
; otherwise return s.
(define (string-remove-last s)
  (cond
    [(> (string-length s) 0)
     (substring s 0 (- (string-length s) 1))]
    [else s]))

(check-expect (string-remove-last "")
              "")
(check-expect (string-remove-last "a")
              "")
(check-expect (string-remove-last "abcdefgh")
              "abcdefg")

; String -> String
; Return s without its first letter if it is non-empty,
; otherwise return s.
(define (string-rest s)
  (cond
    [(> (string-length s) 0)
     (substring s 1 (string-length s))]
    [else s]))

(check-expect (string-rest "")
              "")
(check-expect (string-rest "a")
              "")
(check-expect (string-rest "abcdefgh")
              "bcdefgh")

; String -> String
; Return the first 1String of s if it is non-empty,
; otherwise return s.
(define (string-first s)
  (cond
    [(> (string-length s) 0)
     (substring s 0 1)]
    [else s]))

(check-expect (string-first "")
              "")
(check-expect (string-first "a")
              "a")
(check-expect (string-first "abcdefgh")
              "a")

; String -> String
; Return the last 1String of s if it is non-empty,
; otherwise return s.
(define (string-last s)
  (cond
    [(> (string-length s) 0)
     (substring s (- (string-length s) 1) (string-length s))]
    [else s]))

(check-expect (string-last "")
              "")
(check-expect (string-last "a")
              "a")
(check-expect (string-last "abcdefgh")
              "h")

; Editor -> Boolean
; Check whether the text representation of e
; fits into background
(define (fits? e)
  (if (<= (+ (image-width (text (string-append (editor-pre e) (editor-post e))
                                TEXT-SIZE
                                "black"))
             (image-width CURSOR))
          (image-width BACKGROUND))
      #true
      #false))

(check-expect (fits? (make-editor "" ""))
              #true)
(check-expect (fits? (make-editor (make-string 23 #\o) ""))
              #false)