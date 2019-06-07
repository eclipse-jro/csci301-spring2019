; inputStackTokens (read, A, read, B, C, :=, A, +, B, D)
; parseStack = ("program")


(define id?
  (let ((bool #f))
    (lambda (x)
      (cond
        ((or (or (or (string=? "A" x) (string=? "B" x)) (string=? "C" x)) (string=? "D" x)) (let ((bool #t)) bool))
        ((or (or (or (string=? "E" x) (string=? "F" x)) (string=? "G" x)) (string=? "H" x)) (let ((bool #t)) bool))
        ((or (or (or (string=? "I" x) (string=? "J" x)) (string=? "K" x)) (string=? "L" x)) (let ((bool #t)) bool))
        ((or (or (or (string=? "M" x) (string=? "N" x)) (string=? "O" x)) (string=? "P" x)) (let ((bool #t)) bool))
        ((or (or (or (string=? "Q" x) (string=? "R" x)) (string=? "S" x)) (string=? "T" x)) (let ((bool #t)) bool))
        ((or (or (or (string=? "U" x) (string=? "V" x)) (string=? "W" x)) (string=? "X" x)) (let ((bool #t)) bool))
        ((or (string=? "Y" x) (string=? "Z" x)) (let ((bool #t)) bool))
        (else bool)
        ))))

(define program-table
  (let ((return "ERROR"))
    (lambda (x)
      (cond
        ;((id? x) (let ((return "1"))  return))
        ((string=? "read" x) (let ((return "1"))  return))
        ((string=? "write" x) (let ((return "1"))  return))
        ((string=? "$$" x) (let ((return "1"))  return))
        (else return)
        ))))

(define stmt_list-table
  '(2 0 2 2 0 0 0 0 0 0 0 3))
(define stmt-table
  '(4 0 5 6 0 0 0 0 0 0 0 0))
(define expr-table
  '(7 7 0 0 0 7 0 0 0 0 0 0))
(define term_tail-table
  '(9 0 9 9 0 0 9 8 8 0 0 9))
(define term-table
  '(10 10  0 0 0 10 0 0 0 0 0 0))
(define factor_tail-table
  '(12 0 12 12 0 0 12 12 12 11 11 12))
(define factor-table
  '(14 15 0 0 0 13 0 0 0 0 0 0))
(define add_op-table
  '(0 0 0 0 0 0 0 16 17 0 0 0))
(define mult_op-table
  '(0 0 0 0 0 0 0 0 0 18 19 0))

; 19 production rules. Input production ID. Output right side of ID# rule.
(define production#-to-list
  (let ((result "ERROR"))
    (lambda (x)
      (cond
        ((string=? "1" x) (let ((result '("stmt_list" "$$"))) result))
        ((string=? "2" x) (let ((result '("stmt" "stmt_list"))) result))
        ((string=? "3" x) (let ((result '())) result))
        ((string=? "4" x) (let ((result '("id" ":=" "expr"))) result))
        ((string=? "5" x) (let ((result '("read" "id"))) result))
        ((string=? "6" x) (let ((result '("write" "expr"))) result))
        ((string=? "7" x) (let ((result '("term" "term_tail"))) result))
        ((string=? "8" x) (let ((result '("add_op" "term" "term_tail"))) result))
        ((string=? "9" x) (let ((result '())) result))
        ((string=? "10" x) (let ((result '("factor" "factor_tail"))) result))
        ((string=? "11" x) (let ((result '("mult_op" "factor" "factor_tail"))) result))
        ((string=? "12" x) (let ((result '())) result))
        ((string=? "13" x) (let ((result '("(" "expr" ")"))) result))
        ((string=? "14" x) (let ((result '("id"))) result))
        ((string=? "15" x) (let ((result '("number"))) result))
        ((string=? "16" x) (let ((result '("+"))) result))
        ((string=? "17" x) (let ((result '("-"))) result))
        ((string=? "18" x) (let ((result '("*"))) result))
        ((string=? "19" x) (let ((result '("/"))) result))
        (else result)
        ))))

;;; Define token regonition

(define initial-in-list
  (let ((infile (open-input-file "input1")))
    (let f ((x (read infile)))
      (if (eof-object? x)
          (begin
            (close-input-port infile)
            '())
          (cons x (f (read infile))))))
  )

(define (file-to-char-list dir)
 (call-with-input-file dir
   (lambda (input-port)
     (let recur ((x (read-char input-port)))
       (cond
        ((eof-object? x) '())
        (#t (begin (cons x (recur (read-char input-port))))))))))

(define charlist->stringlist
      (lambda (ls)
        (cond
          ( (null? ls) (let ((done ls)) ls) )
          (else
        (let ((curchar (car ls)))
          (cond
            ;( (equal? ls '()) (curchar) )
            ( (and (equal? curchar #\$) (equal? (car (cdr ls)) #\$)) (let ((result (string curchar (car(cdr ls)))))
                                                                         (cons result (charlist->stringlist (cdr(cdr ls)))) ))
            ( (and (equal? (car ls) #\r) (equal? (car (cdr ls)) #\e)) (let ((result (string (car ls) (car(cdr ls)) (car(cdr(cdr ls))) (car(cdr(cdr(cdr ls))))) ))
                                                                    (cons result (charlist->stringlist (cdr(cdr(cdr(cdr ls)))))) ) )
            ( (and (equal? curchar #\:) (equal? (car (cdr ls)) #\=)) (let ((result (string curchar (car (cdr ls))) ) )
                                                                       (cons result (charlist->stringlist (cdr(cdr ls)))) ) )
            ( (or (equal? curchar #\space) (equal? curchar #\newline)) (charlist->stringlist (cdr ls)) )
            ( (id? (string curchar)) (cons (string curchar) (charlist->stringlist (cdr ls))) )
            ( (or (or (or (equal? curchar #\+) (equal? curchar #\-)) (equal? curchar #\/)) (equal? curchar #\*)) (let ((result (string curchar)))
                                                                                                                   (cons result (charlist->stringlist (cdr ls))) ) )
            ( (integer? (char->integer curchar)) (cons (string curchar) (charlist->stringlist (cdr ls))) )
            ;(else (let ((result "yeet")) result))
            (else ls)
            ))))
        ))

;(charlist->stringlist initial-in-list)

(charlist->stringlist (file-to-char-list "input1"))
;(car(file-to-char-list "test"))
;;;----------------------------------------------------------------  MAIN  --------------------------------------------------------------------------------------------

;; "program-table('read'):"
;; (program-table "read")
;; "production#-to-list('1'):"
;; (production#-to-list "1")