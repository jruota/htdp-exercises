;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex510) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; see fmt --width=0 sample-input.txt
(define WIDTH-ERROR "invalid width")
(define FILE-ERROR "no such file")

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; N String String -> String
; Read all words from the file in-f,
; arrange these words in the given order
; into lines of maximal width w,
; and write these lines to the file out-f.
; If w is smaller than MIN-WIDTH, the lines
; will be formatted to MIN-WIDTH.
(define (fmt w in-f out-f)
  (local ((define WIDTH (if (< w 0) (error WIDTH-ERROR) w))

          ; [List-of [List-of String]] String -> String
          ; Format the paragraphs in lolos to a maximum width
          ; of WIDTH.
          (define (format lolos)
            (local (; [List-of [List-of String]] String -> String
                    ; Format the paragraphs in lolos0 to a maximum width
                    ; of WIDTH.
                    ; The accumulator text collects all formatted
                    ; lines that are not in lolos,
                    ; i.e. (text + lolos0 + lolos) are the complete
                    ; text.
                    (define (format/a lolos0 text)
                      (cond
                        [(empty? lolos0)
                         text]
                        [else
                         (format/a (rest lolos0)
                                   (string-append
                                    text
                                    (format-para (first lolos0))))])))
              ; – IN –
              (format/a lolos "")))

          ; [List-of String] -> String
          ; Format the paragrpah los to a maximum width
          ; of WIDTH.
          (define (format-para los)
            (local (; [List-of String] String String -> String
                    ; Format the paragrpah los0 to a maximum width
                    ; of WIDTH.
                    ; The accumulator line collects the current line,
                    ; and the accumulator para collects the already
                    ; formatted lines that are not in los;
                    ; such that (para + line + los0 + los)
                    ; are the complete initial paragraph.
                    (define (format-para/aa los0 line para)
                      (cond
                        [(empty? los0)
                         (string-append para line "\n")]
                        [else
                         (cond
                           [(and (not (string=? line ""))
                                 (within-width? los0 line))
                            (format-para/aa los0
                                            ""
                                            (string-append para line "\n"))]
                           [(and (not (string=? line ""))
                                 (> (string-length line) WIDTH))
                            (format-para/aa los0
                                            ""
                                            (string-append para line "\n"))]
                           [else
                            (format-para/aa (rest los0)
                                            (if (string=? line "")
                                                (first los0)
                                                (string-append line
                                                               " "
                                                               (first los0)))
                                            para)])])))
              ; – IN –
              (format-para/aa los "" "")))

          ; [List-of [List-of String]] -> [List-of [List-of String]]
          ; Format in such that the overall organization
          ; of the text is preserved, that is it contains
          ; all whitespace and all newlines, but collect
          ; all lines from the same paragraph into one list.
          ; A paragraph break is an empty line.
          (define (format-text in)
            (local (; [List-of [List-of String]]
                    ; [List-of [List-of String]]
                    ; [List-of [List-of String]]
                    ; ->
                    ; [List-of [List-of String]]
                    ; Format input such that the overall organization
                    ; of the text is preserved, that is it contains
                    ; all whitespace and all newlines, but collect
                    ; all lines from the same paragraph into one list.
                    ; A paragraph break is an empty line.
                    ; The accumulator para collects the current paragraph,
                    ; the accumulator result collects the so far formatted
                    ; paragraphs, such that input = result + para.
                    ; The preceding "formula" is just a hint and should
                    ; not be taken literally.
                    (define (format-text/aa input para result)
                      (cond
                        [(and (empty? input) (empty? para))
                         result]
                        [(and (empty? input) (cons? para))
                         (append result (list para))]
                        [(and (cons? input) (empty? para))
                         (if (empty? (first input))
                             (format-text/aa (rest input)
                                     '()
                                     (append result (list '())))
                             (format-text/aa (rest input)
                                             (first input)
                                             result))]
                        [(and (cons? input) (cons? para))
                         (if (empty? (first input))
                             (format-text/aa (rest input)
                                     '()
                                     (append result (list para) (list '())))
                             (format-text/aa (rest input)
                                     (append para (first input))
                                     result))])))
              ; – IN –
              (format-text/aa in '() '())))

          ; [NEList-of String] String -> Boolean
          ; Is str within the boundaries,
          ; i.e. is WIDTH greater than or equal
          ; to the image representation of str
          ; and less than or equal to the image
          ; representation of (string-append str (first los))?
          (define (within-width? los str)
            (and (<= (string-length str) WIDTH)
                 (>= (string-length (string-append str (first los)))
                     WIDTH)))
          
          ; String -> [List-of [List-of String]]
          ; Read the content of file in and produce
          ; it as a list of lists, one per line.
          ; Each line is represented as a list of strings.
          (define (read-input in)
            (if (file-exists? in)
                (read-words/line in)
                (error FILE-ERROR)))

          (define input (read-input in-f))
          (define output (format (format-text input))))
    ; – IN –
    (write-file "sample-output.txt" output)))

(check-error (fmt 80 "does-not-exist.txt" "sample-output.txt")
             FILE-ERROR)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(fmt 12 "sample-input.txt" "sample-output.txt")
