;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex384) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/batch-io)
(require 2htdp/image)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; The function read-plain-xexpr/web eliminates whitespace, while read-xexpr/web
; keeps the whitespace.

; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; An Xexpr is a list: 
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr]

; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

(define-struct data [price delta])
; A StockWorld is a structure: (make-data String String)
; Interpretation:
;     The price and the price change of a stock.

; DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define PREFIX "Https://www.google.com/finance?q=")
(define SIZE 22) ; font size 
 
; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
; String -> StockWorld
; Retrieve the stock price of co and its change every 15s.
(define (stock-alert co)
  (local ((define url (string-append PREFIX co))
          
          ; [StockWorld -> StockWorld]
          ; Retrieve the stock price of co and its change.
          ; The argument __w is ignored.
          (define (retrieve-stock-data __w)
            (local ((define x (read-xexpr/web url)))
              (make-data (get x "price")
                         (get x "priceChange"))))
          
          ; StockWorld -> Image
          ; Render w as an image.
          (define (render-stock-data w)
            (local (; [StockWorld String -> String] -> Image
                    ; Extract a string from w using the selector sel
                    ; and render it in the color specified by col.
                    (define (word sel col)
                      (text (sel w) SIZE col)))
              (overlay (beside (word data-price 'black)
                               (text "  " SIZE 'white)
                               (word data-delta 'red))
                       (rectangle 300 35 'solid 'white)))))
    ; – IN –
    (big-bang (retrieve-stock-data 'no-use)
      [on-tick retrieve-stock-data 15]
      [to-draw render-stock-data])))

; Xexpr String -> String
; Dummy function to make stock-alert work.
(define (get x s)
  (cond
    [(string=? s "price") (number->string (+ 17.09 (- (random 11) 5)))]
    [(string=? s "priceChange") (number->string (+ .06 (- random 5) 2))]
    [else 0]))