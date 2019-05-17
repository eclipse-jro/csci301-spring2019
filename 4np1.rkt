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
;;; https://en.wikibooks.org/wiki/Scheme_Programming/Looping

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


;;;;;;;;;(file->char_list "range.txt")
;(apply string(file->char_list "range.txt"))

;(let* ([data-noNewLine (str-split (apply string(file->char_list "range.txt")) #\newline)]
;       [data-result (str-split data-noNewLine #\space)])
;  data-result)


;(data-noNewLine)
 
;(str-split (apply string(file->char_list "range.txt")) #\newline)
;(str-split (apply string(file->char_list "range.txt")) #\space)


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
(define (squareNum-Sum? num)
  (define (sum-help h)
    (cond [(= h num) #f]
          [(= num (+ (* (floor(sqrt h)) (floor (sqrt h)))
                     (* (floor (sqrt (- num h))) (floor (sqrt (- num h))))  )) #t]
          )))