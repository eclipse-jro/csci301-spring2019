#lang racket

; ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
; // AUTHOR   | Jose Rene Ortega Jr.
; // DATE     | May 17th, 2019
; // PROJECT  | CSCI301 Programming Assignment 1
; // FILE     | 4np1.rkt reads "range.txt" for two ints on a line as input range, and outputs, to a file, prime numbers
; //          | of the form 4n+1 to a file.
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

; splits txt file into characters
(define (file->char_list dir)
 (call-with-input-file dir
   (lambda (input-port)
     (let loop ((x (read-char input-port)))
       (cond 
        ((eof-object? x) '())
        (#t (begin (cons x (loop (read-char input-port))))))))))
; splits a string using a character as delimter
(define (split str ch)
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

;binds range integers for use in main input
(define rangeLow (string->number (first (split (first(split (apply string(file->char_list "range")) #\newline)) #\space))))
(define rangeHigh (string->number (first (reverse (split (first(split (apply string(file->char_list "range")) #\newline)) #\space)))))

;(str-split (first(str-split (apply string(file->char_list "range.txt")) #\newline)) #\space)

;;; STAGE 1: function that prints vals for a range x -> y
; Helper function to append two lists
(define (append ls ls2)
 (if (null? ls) ls2
  (cons (car ls) (append (cdr ls) ls2))))
; returns list of values in specified range
; ls is initally a list of only the lowbound
(define (xy_range_equation ls lowBound highBound)
  (let ([curList (append ls (cons (+ (first (reverse ls)) 1) '()))])
  (cond
    [(= (length curList) (+ (- highBound lowBound) 1)) curList]
    [(< highBound lowBound) (error "InputError: highBound must be greater than lowBound")]
    [(= highBound lowBound) (list(first curList))]
    [else (xy_range_equation curList lowBound highBound)])))

;;; STAGE 2: Call the function from stage 1 and return to ones that are primes
        ;map might be useful here
;Call the function from stage 1 and return to ones that are primes

; 3 helper functions to determine if a number is prime
(define (isPrime? num)
  (cond
    [(<= num 1) #f]
    [(or (= num 2) (= num 3)) #t]
    [else (andmap magicMath (xy_range_equation '(2) 2 (- num 1)) (makeSameNumList (list num)))]))
; checks factors
(define (magicMath factor maybePrime)
  (cond
    [(= (modulo maybePrime factor) 0) #f]
    [else #t]))
; utility function to iterate through andmap lists
(define (makeSameNumList ls)
  (let ([sameNumList (append ls (list (first ls)))])
    (cond
      [(= (length ls) (- (first ls) 3)) sameNumList]
      [else (makeSameNumList sameNumList)])))

;; Returns a list of primes from a list of integers
(define (primes rangeList)
  (filter isPrime? rangeList))
  
;;; STAGE 3: Take the primes produced by the function in stage 2 and determine the two squares
;;;          that sum to the prime of the form '4n + 1"
;;; NEEDS WORK

; defines output file, will replace if it exists
(define out (open-output-file "primes4np1" #:exists 'replace))

; give squares if true
(define (giveSquares num)    
(for ([i (xy_range_equation '(0) 0 num )])
  (for ([j (xy_range_equation '(0) 0 num)])
    (cond [(and (= (+ (* i i) (* j j)) num) (< i j))
              (write num out) (display " " out) (write i out) (display " " out) (write j out) (newline out) (newline out)]
    ))))

; boolean to determine if it is in this form, helps filter range values
(define (4n+1? n)
    (cond
      [(= (modulo (- n 1) 4) 0) #t]
      [else #f]))

; main function
( define (main low high)
   (for ([i (primes (filter 4n+1? (xy_range_equation (list low) low high)))])
     (giveSquares i))
   )

(main rangeLow rangeHigh)
(close-output-port out)