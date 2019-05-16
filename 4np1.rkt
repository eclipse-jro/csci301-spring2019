#lang racket

; ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
; // AUTHOR   | Jose Rene Ortega Jr.
; // DATE     | May 17th, 2019
; // PROJECT  | CSCI301 Programming Assignment 1
; // FILE     | 4np1.rkt [description].
; ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

; Final Objective:
;    Create a function that will find the primes of the form "4n + 1" and show the prime  
;    and the two unique squares that sum to that prime.

; Three Stages of this program:
; 1) Create a function that will produce numbers of the form "4n + 1"
; 2) Call the function from stage 1 and return to ones that are primes
; 3) Take the primes produced by the function in stage 2 and determine the two squares
;    that sum to the prime of the form '4n + 1"

; This program reads file "range" as input which contains two integers; lowBound & highBound
; This program then outputs to a file named "primes4np1"
; ------------------------------------------------------------------------------------------------------------------------

;;; https://groups.csail.mit.edu/mac/ftpdir/scheme-7.4/doc-html/scheme_7.html
;;; https://docs.racket-lang.org/reference/let.html
;;; https://docs.racket-lang.org/reference/reader.html#%28part._parse-symbol%29
;;; https://gist.github.com/matthewp/2324447
;;; https://www.scheme.com/tspl3/binding.html#./binding:h1
;;; 


; splits txt file into characters
(define (file->char_list path)
 (call-with-input-file path
   (lambda (input-port)
     (let loop ((x (read-char input-port)))
       (cond 
        ((eof-object? x) '())
        (#t (begin (cons x (loop (read-char input-port))))))))))

; splits a string using a character as delimter
(define (str-split str ch)
  (let ((len (string-length str)))
    (letrec
      ((split
        (lambda (a b)
          (cond
            ((>= b len) (if (= a b) '() (cons (substring str a b) '())))
              ((char=? ch (string-ref str b)) (if (= a b)
                (split (+ 1 a) (+ 1 b))
                  (cons (substring str a b) (split b b))))
                (else (split a (+ 1 b)))))))
(split 0 0))))


(file->char_list "range.txt")
;(apply string(file->char_list "range.txt"))

(let* ([data-noNewLine (str-split (apply string(file->char_list "range.txt")) #\newline)]
       [data-result (str-split data-noNewLine #\space)])
  data-result)


;(data-noNewLine)
 
;(str-split (apply string(file->char_list "range.txt")) #\newline)
;(str-split (apply string(file->char_list "range.txt")) #\space)


;;; STAGE 1: function that prints vals for a range x -> y
(define xy_range_equation
  (lambda (lower upper)
    (do 

;;; STAGE 2:
        ;map might be useful here


;;; STAGE 3:
