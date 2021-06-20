;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex402) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; While going through the associations list, the functions does not need to know
; about the S-expression. It simply unpacks the associations and passes them on
; to subst together with the assumed atomic value that is the S-expression.