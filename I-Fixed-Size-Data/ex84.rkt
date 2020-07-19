;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex84) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)    ; for key=? function

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

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Editor KeyEvent -> Editor
;; Add ke to the end of the pre-field of e.
;(define (edit e ke)
;  (cond
;    [(key=? ke "\b")
;     (make-editor (string-remove-last (editor-pre e))
;                  (editor-post e))]
;    [(key=? ke "\t") e]
;    [(key=? ke "\r") e]
;    [(key=? ke "left")
;     (make-editor (string-remove-last (editor-pre e))
;                  (string-append (string-last (editor-pre e))
;                                 (editor-post e)))]
;    [(key=? ke "right")
;     (make-editor (string-append (editor-pre e)
;                                 (string-first (editor-post e)))
;                  (string-rest (editor-post e)))]
;    [else
;     (make-editor (string-append (editor-pre e)
;                                 ke)
;                  (editor-post e))]))

; NOTE - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; The above version would not work for key-events like "shift", as it would
; append them to the pre-field instead of ignoring them. One has to check
; for the length of the key-event.
; END NOTE - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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
         (make-editor (string-append (editor-pre e) ke)
                      (editor-post e))])]))
       

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