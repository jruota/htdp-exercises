;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex441) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [else
     (append (quick-sort< (smallers alon (first alon)))
             (list (first alon))
             (quick-sort< (largers alon (first alon))))]))

(check-expect (quick-sort< '()) '())
(check-expect (quick-sort< (list 11 8 14 7)) (list 7 8 11 14))
(check-expect (quick-sort< (list 11 9 2 18 12 14 4 1))
              (list 1 2 4 9 11 12 14 18))

; [List-of Number] Number -> [List-of Number]
; Return all numbers from lon that are less
; than n.
(define (smallers lon n)
  (cond
    [(empty? lon) '()]
    [else
     (if (< (first lon) n)
         (cons (first lon) (smallers (rest lon) n))
         (smallers (rest lon) n))]))

(check-expect (smallers (list 11 8 14 7) 11)
              (list 8 7))
(check-expect (smallers (list 11 9 2 18 12 14 4 1) 11)
              (list 9 2 4 1))

; [List-of Number] Number -> [List-of Number]
; Return all numbers from lon that are greater
; than n.
(define (largers lon n)
  (cond
    [(empty? lon) '()]
    [else
     (if (> (first lon) n)
         (cons (first lon) (largers (rest lon) n))
         (largers (rest lon) n))]))

(check-expect (largers (list 11 8 14 7) 11)
              (list 14))
(check-expect (largers (list 11 9 2 18 12 14 4 1) 11)
              (list 18 12 14))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;(quick-sort< (list 10 6 8 9 14 12 3 11 14 16 2))
;==
;(append (quick-sort< (list 6 8 9 3 2))
;        (list 10)
;        (quick-sort< (list 14 12 11 14 16)))
;==
;(append (append (quick-sort< (list 3 2))
;                (list 6)
;                (quick-sort< (list 8 9)))
;        (list 10)
;        (append (quick-sort< (list 12 11))
;                (list 14)                   ; one 14 dropped here
;                (quick-sort< (list 16))))
;==
;(append (append (append (quick-sort< (list 2))
;                        (list 3)
;                        (quick-sort< '()))
;                (list 6)
;                (append (quick-sort< '())
;                        (list 8)
;                        (quick-sort< (list 9))))
;        (list 10)
;        (append (append (quick-sort< (list 11))
;                        (list 12)
;                        (quick-sort< '()))
;                (list 14)
;                (append (quick-sort< '())
;                        (list 16)
;                        (quick-sort< '()))))
;==
;(append (append (append (append (quick-sort< '())
;                                (list 2)
;                                (quick-sort '()))
;                        (list 3)
;                        '())
;                (list 6)
;                (append '()
;                        (list 8)
;                        (append (quick-sort< '())
;                                (list 9)
;                                (quick-sort< '()))))
;        (list 10)
;        (append (append (append (quick-sort< '())
;                                (list 11)
;                                (quick-sort< '()))
;                        (list 12)
;                        '())
;                (list 14)
;                (append '()
;                        (list 16)
;                        '())))
;==
;(append (append (append (append '()
;                                (list 2)
;                                '())
;                        (list 3)
;                        '())
;                (list 6)
;                (append '()
;                        (list 8)
;                        (append '()
;                                (list 9)
;                                '())))
;        (list 10)
;        (append (append (append '()
;                                (list 11)
;                                '())
;                        (list 12)
;                        '())
;                (list 14)
;                (append '()
;                        (list 16)
;                        '())))
;==
;(append (append (append (list 2)
;                        (list 3)
;                        '())
;                (list 6)
;                (append '()
;                        (list 8)
;                        (list 9)))
;        (list 10)
;        (append (append (list 11)
;                        (list 12)
;                        '())
;                (list 14)
;                (list 16)))
;==
;(append (append (list 2 3)
;                (list 6)
;                (list 8 9))
;        (list 10)
;        (list 11 12)
;        (list 14)
;        (list 16))
;==
;(append (list 2 3 6 8 9)
;        (list 10)
;        (list 11 12)
;        (list 14)
;        (list 16))
;==
;(list 2 3 6 8 9 10 11 12 14 16)

; 10 "append" calls                  -> (number of unique numbers)
; 20 (recursive) "quick-sort<" calls -> (* 2 (number of unique numbers)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;(quick-sort< (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
;==
;(append
; (quick-sort< '())
; (list 1)
; (quick-sort< (list 2 3 4 5 6 7 8 9 10 11 12 13 14)))
;==
;(append
; '()
; (list 1)
; (append
;  (quick-sort< '())
;  (list 2)
;  (quick-sort< (list 3 4 5 6 7 8 9 10 11 12 13 14))))
;==
;(append
; '()
; (list 1)
; (append
;  '()
;  (list 2)
;  (append
;   (quick-sort< '())
;   (list 3)
;   (quick-sort< (list 4 5 6 7 8 9 10 11 12 13 14)))))
;==
;(append
; '()
; (list 1)
; (append
;  '()
;  (list 2)
;  (append
;   '()
;   (list 3)
;   (append
;    (quick-sort< '())
;    (list 4)
;    (quick-sort< (list 5 6 7 8 9 10 11 12 13 14))))))
;==
;(append
; '()
; (list 1)
; (append
;  '()
;  (list 2)
;  (append
;   '()
;   (list 3)
;   (append
;    '()
;    (list 4)
;    (append
;     (quick-sort< '())
;     (list 5)
;     (quick-sort< (list 6 7 8 9 10 11 12 13 14)))))))
;==
;(append
; '()
; (list 1)
; (append
;  '()
;  (list 2)
;  (append
;   '()
;   (list 3)
;   (append
;    '()
;    (list 4)
;    (append
;     '()
;     (list 5)
;     (append
;      (quick-sort< '())
;      (list 6)
;      (quick-sort< (list 7 8 9 10 11 12 13 14))))))))
;==
;(append
; '()
; (list 1)
; (append
;  '()
;  (list 2)
;  (append
;   '()
;   (list 3)
;   (append
;    '()
;    (list 4)
;    (append
;     '()
;     (list 5)
;     (append
;      '()
;      (list 6)
;      (append
;       (quick-sort< '())
;       (list 7)
;       (quick-sort< (list 8 9 10 11 12 13 14)))))))))
;==
;(append
; '()
; (list 1)
; (append
;  '()
;  (list 2)
;  (append
;   '()
;   (list 3)
;   (append
;    '()
;    (list 4)
;    (append
;     '()
;     (list 5)
;     (append
;      '()
;      (list 6)
;      (append
;       '()
;       (list 7)
;       (append
;        (quick-sort< '())
;        (list 8)
;        (quick-sort< (list 9 10 11 12 13 14))))))))))
;==
;(append
; '()
; (list 1)
; (append
;  '()
;  (list 2)
;  (append
;   '()
;   (list 3)
;   (append
;    '()
;    (list 4)
;    (append
;     '()
;     (list 5)
;     (append
;      '()
;      (list 6)
;      (append
;       '()
;       (list 7)
;       (append
;        '()
;        (list 8)
;        (append
;         (quick-sort< '())
;         (list 9)
;         (quick-sort< (list 10 11 12 13 14)))))))))))
;==
;(append
; '()
; (list 1)
; (append
;  '()
;  (list 2)
;  (append
;   '()
;   (list 3)
;   (append
;    '()
;    (list 4)
;    (append
;     '()
;     (list 5)
;     (append
;      '()
;      (list 6)
;      (append
;       '()
;       (list 7)
;       (append
;        '()
;        (list 8)
;        (append
;         '()
;         (list 9)
;         (append
;          (quick-sort< '())
;          (list 10)
;          (quick-sort< (list 11 12 13 14))))))))))))
;==
;(append
; '()
; (list 1)
; (append
;  '()
;  (list 2)
;  (append
;   '()
;   (list 3)
;   (append
;    '()
;    (list 4)
;    (append
;     '()
;     (list 5)
;     (append
;      '()
;      (list 6)
;      (append
;       '()
;       (list 7)
;       (append
;        '()
;        (list 8)
;        (append
;         '()
;         (list 9)
;         (append
;          '()
;          (list 10)
;          (append
;           (quick-sort< '())
;           (list 11)
;           (quick-sort< (list 12 13 14)))))))))))))
;==
;(append
; '()
; (list 1)
; (append
;  '()
;  (list 2)
;  (append
;   '()
;   (list 3)
;   (append
;    '()
;    (list 4)
;    (append
;     '()
;     (list 5)
;     (append
;      '()
;      (list 6)
;      (append
;       '()
;       (list 7)
;       (append
;        '()
;        (list 8)
;        (append
;         '()
;         (list 9)
;         (append
;          '()
;          (list 10)
;          (append
;           '()
;           (list 11)
;           (append
;            (quick-sort< '())
;            (list 12)
;            (quick-sort< (list 13 14))))))))))))))
;==
;(append
; '()
; (list 1)
; (append
;  '()
;  (list 2)
;  (append
;   '()
;   (list 3)
;   (append
;    '()
;    (list 4)
;    (append
;     '()
;     (list 5)
;     (append
;      '()
;      (list 6)
;      (append
;       '()
;       (list 7)
;       (append
;        '()
;        (list 8)
;        (append
;         '()
;         (list 9)
;         (append
;          '()
;          (list 10)
;          (append
;           '()
;           (list 11)
;           (append
;            '()
;            (list 12)
;            (append
;             (quick-sort< '())
;             (list 13)
;             (quick-sort< (list 14)))))))))))))))
;==
;(append
; '()
; (list 1)
; (append
;  '()
;  (list 2)
;  (append
;   '()
;   (list 3)
;   (append
;    '()
;    (list 4)
;    (append
;     '()
;     (list 5)
;     (append
;      '()
;      (list 6)
;      (append
;       '()
;       (list 7)
;       (append
;        '()
;        (list 8)
;        (append
;         '()
;         (list 9)
;         (append
;          '()
;          (list 10)
;          (append
;           '()
;           (list 11)
;           (append
;            '()
;            (list 12)
;            (append
;             '()
;             (list 13)
;             (append
;              (quick-sort< '())
;              (list 14)
;              (quick-sort< '())))))))))))))))
;==
;(append
; '()
; (list 1)
; (append
;  '()
;  (list 2)
;  (append
;   '()
;   (list 3)
;   (append
;    '()
;    (list 4)
;    (append
;     '()
;     (list 5)
;     (append
;      '()
;      (list 6)
;      (append
;       '()
;       (list 7)
;       (append
;        '()
;        (list 8)
;        (append
;         '()
;         (list 9)
;         (append
;          '()
;          (list 10)
;          (append
;           '()
;           (list 11)
;           (append
;            '()
;            (list 12)
;            (append
;             '()
;             (list 13)
;             (append
;              '()
;              (list 14)
;              '()))))))))))))))
;==
;(append
; '()
; (list 1)
; (append
;  '()
;  (list 2)
;  (append
;   '()
;   (list 3)
;   (append
;    '()
;    (list 4)
;    (append
;     '()
;     (list 5)
;     (append
;      '()
;      (list 6)
;      (append
;       '()
;       (list 7)
;       (append
;        '()
;        (list 8)
;        (append
;         '()
;         (list 9)
;         (append
;          '()
;          (list 10)
;          (append
;           '()
;           (list 11)
;           (append
;            '()
;            (list 12)
;            (append
;             '()
;             (list 13)
;             (list 14))))))))))))))
;==
;(append
; '()
; (list 1)
; (append
;  '()
;  (list 2)
;  (append
;   '()
;   (list 3)
;   (append
;    '()
;    (list 4)
;    (append
;     '()
;     (list 5)
;     (append
;      '()
;      (list 6)
;      (append
;       '()
;       (list 7)
;       (append
;        '()
;        (list 8)
;        (append
;         '()
;         (list 9)
;         (append
;          '()
;          (list 10)
;          (append
;           '()
;           (list 11)
;           (append
;            '()
;            (list 12)
;            (list 13 14)))))))))))))
;==
;...
;==
;(list 1 2 3 4 5 6 7 8 9 10 11 12 13 14)

; 14 "append" calls                  -> (number of unique numbers)
; 28 (recursive) "quick-sort<" calls -> (* 2 (number of unique numbers)