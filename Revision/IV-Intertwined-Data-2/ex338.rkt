;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex338) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define TS (create-dir "TS"))
(define TS-2 (create-dir "TS-2"))

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; from ex336.rkt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Dir.v3 -> N
; Count the number of files in dir.
(define (how-many dir)
  (local ((define FILES (dir-files dir))
          (define DIRS (dir-dirs dir))

          ; File* -> N
          ; Count the number of files in f*.
          (define (files-how-many f*)
            (length f*))

          ; Dir* -> N
          ; Count the number of files contained
          ; in all directories in d*.
          (define (dirs-how-many d*)
            (cond
              [(empty? d*) 0]
              [else
               (+ (how-many (first d*))
                  (dirs-how-many (rest d*)))])))
    ; – IN –
    (+ (files-how-many FILES)
       (dirs-how-many DIRS))))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(how-many TS)   ; 7
(how-many TS-2) ; 9
