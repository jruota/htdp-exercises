;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex509) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; Editor Integer Integer MouseEvent -> Editor
; Handle mouse events, given some editor.
(define (mouse-eh ed x y me)
  (cond
    [(mouse=? me "button-down")
     (split (append (reverse (editor-pre ed)) (editor-post ed))
            x)]
    [else
     ed]))

(check-expect (mouse-eh (create-editor "hello " "world") 199 10 "enter")
              (create-editor "hello " "world"))
(check-expect (mouse-eh (create-editor "hello " "world") 199 10 "button-down")
              (create-editor "hello world" ""))

; [List-of 1String] N -> Editor
; Produce an editor (make-editor p s)
; such that (1) p and s make up ed and
; (2) x is larger than the image of p
; and smaller than the image of p
; extended with the first 1String on s (if any).
(define (split ed x)
  (local (; String [List-of 1String] -> Editor
          ; Return an editor such that the cursor is placed
          ; at position x in the image representation of the
          ; editor.
          ; Here str contains ther first n 1Strings from
          ; ed that are not in lo1s,
          ; i.e. ed == (implode (append (explode str) lo1s)).
          (define (split-helper str lo1s)
            (cond
              [(empty? lo1s)
               (create-editor str "")]
              [else
               (cond
                 [(within-boundaries? str lo1s)
                  (create-editor str (implode lo1s))]
                 [else
                  (split-helper (string-append str (first lo1s))
                                (rest lo1s))])]))

          ; String [List-of 1String] -> Boolean
          ; Is x within the boundaries,
          ; i.e. is x greater than or equal
          ; to the image representation of s1
          ; and less than or equal to the image
          ; representation of (string-append s1 (first s2))?
          (define (within-boundaries? s1 s2)
            (and (<= (image-width (editor-text (explode s1))) x)
                 (>= (image-width
                      (editor-text
                       (explode
                        (string-append s1 (first s2)))))
                     x))))
    ; – IN –
    (split-helper "" ed)))

(check-satisfied (split (explode "") 0)
                 (conditions-met? "" "" 0))
(check-satisfied (split (explode "") 50)
                 (conditions-met? "" "" 0))

(check-satisfied (split (explode "hello world") 0)
                 (conditions-met? "" "hello world" 0))
(check-satisfied (split (explode "hello world") 24)
                 (conditions-met? "hel" "lo world" 24))
(check-satisfied (split (explode "hello world") 150)
                 (conditions-met? "hello world" "" 78))

; NOTE -----------
; for testing only
; END NOTE -------
; String String N -> [Editor -> Boolean]
; Return a function that tests whether the
; conditions as stated in the exercise are met.
; The argument x is the position of the cursor.
(define (conditions-met? p s x)
  (local (; Editor -> Boolean
          ; Does ed satisfy the conditions
          ; from the exercise statement?
          (define (cm? ed)
            (and (editor? ed)
                 (string=? (string-append p s)
                           (string-append (implode (reverse (editor-pre ed)))
                                          (implode (editor-post ed))))
                 (<= (image-width (editor-text (explode p)))
                     x
                     (image-width
                      (editor-text
                       (append (explode p)
                               (if (> (string-length s) 0)
                                   (list (first (explode s)))
                                   (list s)))))))))
    ; – IN –
    cm?))

; from ex180.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; String -> Editor
; launches the editor given some initial string 
(define (main s)
   (big-bang (create-editor s "")
     [on-key editor-kh]
     [to-draw editor-render]
     [on-mouse mouse-eh]))           ; different form exercise 180

; Editor -> Image
; renders an editor as an image of the two texts 
; separated by the cursor
(define (editor-render e)
  (place-image/align
    (beside (editor-text (reverse (editor-pre e)))
            CURSOR
            (editor-text (editor-post e)))
    1 1
    "left" "top"
    MT))

(check-expect (editor-render (create-editor "pre" "post"))
              (place-image/align
               (beside (text "pre" FONT-SIZE FONT-COLOR)
                       CURSOR
                       (text "post" FONT-SIZE FONT-COLOR))
               1 1
               "left" "top"
               MT))
(check-expect (editor-render (create-editor "" "post"))
              (place-image/align
               (beside CURSOR
                       (text "post" FONT-SIZE FONT-COLOR))
               1 1
               "left" "top"
               MT))
(check-expect (editor-render (create-editor "pre" ""))
              (place-image/align
               (beside (text "pre" FONT-SIZE FONT-COLOR)
                       CURSOR)
               1 1
               "left" "top"
               MT))
(check-expect (editor-render (create-editor "" ""))
              (place-image/align
               CURSOR
               1 1
               "left" "top"
               MT))

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

; Lo1s -> Image
; renders a list of 1Strings as a text image 
(define (editor-text s)
  (text (lo1s->string s) FONT-SIZE FONT-COLOR))

(check-expect (editor-text (cons "p" (cons "o" (cons "s" (cons "t" '())))))
              (text "post" FONT-SIZE FONT-COLOR))
(check-expect (editor-text '())
              (text "" FONT-SIZE FONT-COLOR))

; Lo1s -> String
; Return a string by appending the
; elements of lo1s to each other.
(define (lo1s->string lo1s)
  (cond
    [(empty? lo1s) ""]
    [(cons? lo1s)
     (string-append (first lo1s)
                    (lo1s->string (rest lo1s)))]))

(check-expect (lo1s->string '())
              "")
(check-expect (lo1s->string (explode "hello"))
              "hello")

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

; from ex172.rkt (exercise 172) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; uses reverse instead of rev

; String String -> Editor
; Create an editor from s1 and s2.
(define (create-editor s1 s2)
  (make-editor (reverse (explode s1))
               (explode s2)))
