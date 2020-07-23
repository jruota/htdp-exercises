;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex104) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DATA DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-struct vehicle [passengers license-plate fuel-consumption])
; A Vehicle is a structure:
;     (make-vehicle Number String Number)
; Interpretation:
;     The amount of passengers that fit inside the vehicle,
;     the vehicles license plate number and its fuel
;     consumption in miles per gallon.

; FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Vehicle -> ???
; Template function for dealing with vehicles.
(define (vehicle-tamplate v)
  (... (vehicle-passengers v) ...
       (vehicle-license-plate v) ...
       (vehicle-fuel-consumption v) ...))