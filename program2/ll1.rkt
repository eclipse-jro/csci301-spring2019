;;; Define all ids in this language
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

;;; Define associated row-column intersections for input token and top-of-stack non-terminal
(define program-table
  (let ((return "ERROR"))
    (lambda (x)
      (cond
        ((id? x) (let ((return "1"))  return))
        ((string=? "read" x) (let ((return "1"))  return))
        ((string=? "write" x) (let ((return "1"))  return))
        ((string=? "" x) (let ((return "1"))  return))
        ((string=? "$$" x) (let ((return "1"))  return))
        (else return)
        ))))
(define stmt_list-table
  (let ((return "ERROR"))
    (lambda (x)
      (cond
        ((id? x) (let ((return "2"))  return))
        ((string=? "read" x) (let ((return "2"))  return))
        ((string=? "write" x) (let ((return "2"))  return))
        ((string=? "" x) (let ((return "3"))  return))
        ((string=? "$$" x) (let ((return "3"))  return))
        (else return)
        ))))
(define stmt-table
  (let ((return "ERROR"))
    (lambda (x)
      (cond
        ((id? x) (let ((return "4"))  return))
        ((string=? "read" x) (let ((return "5"))  return))
        ((string=? "write" x) (let ((return "6"))  return))
        (else return)
        ))))
(define expr-table
  (let ((return "ERROR"))
    (lambda (x)
      (cond
        ((id? x) (let ((return "7"))  return))
        ((integer? x) (let ((return "7"))  return))
        ((string=? "(" x) (let ((return "7"))  return))
        (else return)
        ))))
(define term_tail-table
  (let ((return "ERROR"))
    (lambda (x)
      (cond
        ((id? x) (let ((return "9"))  return))
        ((string=? "read" x) (let ((return "9"))  return))
        ((string=? "write" x) (let ((return "9"))  return))
        ((string=? ")" x) (let ((return "9"))  return))
        ((string=? "+" x) (let ((return "8"))  return))
        ((string=? "-" x) (let ((return "8"))  return))
        ((string=? "" x) (let ((return "9"))  return))
        ((string=? "$$" x) (let ((return "9"))  return))
        (else return)
        ))))
(define term-table
  (let ((return "ERROR"))
    (lambda (x)
      (cond
        ((id? x) (let ((return "10"))  return))
        ((integer? x) (let ((return "10"))  return))
        ((string=? "(" x) (let ((return "10"))  return))
        (else return)
        ))))
(define factor_tail-table
  (let ((return "ERROR"))
    (lambda (x)
      (cond
        ((id? x) (let ((return "12"))  return))
        ((string=? "read" x) (let ((return "12"))  return))
        ((string=? "write" x) (let ((return "12"))  return))
        ((string=? ")" x) (let ((return "12"))  return))
        ((string=? "+" x) (let ((return "12"))  return))
        ((string=? "-" x) (let ((return "12"))  return))
        ((string=? "*" x) (let ((return "11"))  return))
        ((string=? "*" x) (let ((return "11"))  return))
        ((string=? "/" x) (let ((return "11"))  return))
        ((string=? "" x) (let ((return "12"))  return))
        ((string=? "$$" x) (let ((return "12"))  return))
        (else return)
        ))))
(define factor-table
  (let ((return "ERROR"))
    (lambda (x)
      (cond
        ((id? x) (let ((return "14"))  return))
        ((integer? (string->number x)) (let ((return "15"))  return))
        ((string=? "(" x) (let ((return "13"))  return))
        (else return)
        ))))
(define add_op-table
  (let ((return "ERROR"))
    (lambda (x)
      (cond
        ((string=? "+" x) (let ((return "16"))  return))
        ((string=? "-" x) (let ((return "17"))  return))
        (else return)
        ))))
(define mult_op-table
  (let ((return "ERROR"))
    (lambda (x)
      (cond
        ((string=? "*" x) (let ((return "18"))  return))
        ((string=? "/" x) (let ((return "19"))  return))
        (else return)
        ))))

;;; Define all 19 production rules:
;;; Input production ID <--> Output right side of ID# rule
(define production#-to-list
  (let ((result "ERROR"))
    (lambda (x)
      (cond
        ((string=? "1" x) (let ((result '("stmt_list" "$$"))) result))
        ((string=? "2" x) (let ((result '("stmt" "stmt_list"))) result))
        ((string=? "3" x) (let ((result '(""))) result))
        ((string=? "4" x) (let ((result '("id" ":=" "expr"))) result))
        ((string=? "5" x) (let ((result '("read" "id"))) result))
        ((string=? "6" x) (let ((result '("write" "expr"))) result))
        ((string=? "7" x) (let ((result '("term" "term_tail"))) result))
        ((string=? "8" x) (let ((result '("add_op" "term" "term_tail"))) result))
        ((string=? "9" x) (let ((result '(""))) result))
        ((string=? "10" x) (let ((result '("factor" "factor_tail"))) result))
        ((string=? "11" x) (let ((result '("mult_op" "factor" "factor_tail"))) result))
        ((string=? "12" x) (let ((result '(""))) result))
        ((string=? "13" x) (let ((result '("(" "expr" ")"))) result))
        ((string=? "14" x) (let ((result '("id"))) result))
        ((string=? "15" x) (let ((result '("number"))) result))
        ((string=? "16" x) (let ((result '("+"))) result))
        ((string=? "17" x) (let ((result '("-"))) result))
        ((string=? "18" x) (let ((result '("*"))) result))
        ((string=? "19" x) (let ((result '("/"))) result))
        (else result)
        ))))

;;; Print parse stack and input stream to associated out-files... return right side of  the production rule as a list
(define (get_token_return parse-string input-string)
  (write parse-string out_parse) (newline out_parse)
  (write input-string out_stream) (newline out_stream)
  (cond
    ( (or (string=? parse-string input-string) (and (id? input-string) (equal? "id" parse-string) ))
      (display "match " out_comment)(write input-string out_comment)(newline out_comment)
      '())
    ( (string=? "program" parse-string)
      (display "predict " out_comment)(write (program-table input-string) out_comment)(newline out_comment)
      (production#-to-list (program-table input-string)))
    ( (string=? "stmt_list" parse-string)
      (display "predict " out_comment)(write (stmt_list-table input-string) out_comment)(newline out_comment)
      (production#-to-list (stmt_list-table input-string)))
    ( (string=? "stmt" parse-string)
      (display "predict " out_comment)(write (stmt-table input-string) out_comment)(newline out_comment)
      (production#-to-list (stmt-table input-string)))
    ( (string=? "expr" parse-string)
      (display "predict " out_comment)(write (expr-table input-string) out_comment)(newline out_comment)
      (production#-to-list (expr-table input-string)))
    ( (string=? "term_tail" parse-string)
      (display "predict " out_comment)(write (term_tail-table input-string) out_comment)(newline out_comment)
      (production#-to-list (term_tail-table input-string)))
    ( (string=? "term" parse-string)
      (display "predict " out_comment)(write (term-table input-string) out_comment)(newline out_comment)
      (production#-to-list (term-table input-string)))
    ( (string=? "factor_tail" parse-string)
      (display "predict " out_comment)(write (factor_tail-table input-string) out_comment)(newline out_comment)
      (production#-to-list (factor_tail-table input-string)))
    ( (string=? "factor" parse-string)
      (display "predict " out_comment)(write (factor-table input-string) out_comment)(newline out_comment)
      (production#-to-list (factor-table input-string)))
    ( (string=? "add_op" parse-string)
      (display "predict " out_comment)(write (add_op-table input-string) out_comment)(newline out_comment)
      (production#-to-list (add_op-table input-string)))
    ( (string=? "mult_op" parse-string)
      (display "predict " out_comment)(write (mult_op-table input-string) out_comment)(newline out_comment)
      (production#-to-list (mult_op-table input-string)))
    ))

;;; PROBS NOT NEEDED
;(define initial-in-list
;  (let ((infile (open-input-file "input1")))
;    (let f ((x (read infile)))
;      (if (eof-object? x)
;          (begin
;            (close-input-port infile)
;            '())
;          (cons x (f (read infile))))))
;  )

;;; Specify a filename in the working directory and convert to char list
(define (file-to-char-list dir)
 (call-with-input-file dir
   (lambda (input-port)
     (let recur ((x (read-char input-port)))
       (cond
        ((eof-object? x) '())
        (#t (begin (cons x (recur (read-char input-port))))))))))

;;; Converts char list to string list
(define charlist->stringlist
      (lambda (ls)
        (cond
          ( (null? ls) (let ((done ls)) ls) )
          (else
        (let ((curchar (car ls)))
          (cond
            ( (and (equal? curchar #\$) (equal? (car (cdr ls)) #\$)) (let ((result (string curchar (car(cdr ls)))))
                                                                         (cons result (charlist->stringlist (cdr(cdr ls)))) ))
            ( (and (equal? (car ls) #\r) (equal? (car (cdr ls)) #\e)) (let ((result (string (car ls) (car(cdr ls)) (car(cdr(cdr ls))) (car(cdr(cdr(cdr ls))))) ))
                                                                    (cons result (charlist->stringlist (cdr(cdr(cdr(cdr ls)))))) ) )
            ( (and (equal? (car ls) #\w) (equal? (car (cdr ls)) #\r)) (let ((result (string (car ls) (car(cdr ls)) (car(cdr(cdr ls))) (car(cdr(cdr(cdr ls)))) (car(cdr(cdr(cdr(cdr ls)))))) ))
                                                                    (cons result (charlist->stringlist (cdr(cdr(cdr(cdr(cdr ls))))))) ) )
            ( (and (equal? curchar #\:) (equal? (car (cdr ls)) #\=)) (let ((result (string curchar (car (cdr ls))) ) )
                                                                       (cons result (charlist->stringlist (cdr(cdr ls)))) ) )
            ( (or (equal? curchar #\space) (equal? curchar #\newline)) (charlist->stringlist (cdr ls)) )
            ( (id? (string curchar)) (cons (string curchar) (charlist->stringlist (cdr ls))) )
            ( (or (or (or (equal? curchar #\+) (equal? curchar #\-)) (equal? curchar #\/)) (equal? curchar #\*)) (let ((result (string curchar)))
                                                                                                                   (cons result (charlist->stringlist (cdr ls))) ) )
            ( (integer? (char->integer curchar)) (cons (string curchar) (charlist->stringlist (cdr ls))) )
            (else ls)
            ))))
        ))

;;; Define three output files for computation trace
(define out_parse (open-output-file "parsestack" #:exists 'replace))
(define out_stream (open-output-file "inputstream" #:exists 'replace))
(define out_comment (open-output-file "comment" #:exists 'replace))

;;; MAIN FUNCTION
(define (run parse_stack input_stack)
  (cond
    ((equal? (car parse_stack) "$$" ) (get_token_return "" "")) ; end state
    ( (or (equal? (car parse_stack) (car input_stack)) (and (id? (car input_stack)) (equal? "id" (car parse_stack))) ) 
      (let ((return-stack (cdr parse_stack) ))
        (get_token_return (car parse_stack) (car input_stack))
        (run return-stack (cdr input_stack) ) ))
    ( (not (equal? (car parse_stack)(car input_stack)))
      (let ((return-stack (append (get_token_return (car parse_stack) (car input_stack) ) (cdr parse_stack)) ))
        (run return-stack input_stack ) ) )
    (else "ERROR!!!!")
    ))

;(charlist->stringlist initial-in-list)
;(charlist->stringlist (file-to-char-list "test"))
;;;"input1 below:"
;;;(charlist->stringlist (file-to-char-list "input1"))
;;"input2 below:"
;;(charlist->stringlist (file-to-char-list "input2"))
;;"input3 below:"
;;(charlist->stringlist (file-to-char-list "input3"))

;;;----------------------------------------------------------------  MAIN  --------------------------------------------------------------------------------------------
(display "initial stack contents" out_comment)
(newline out_comment)
(run '("program") (charlist->stringlist (file-to-char-list "input1")))

;(get_token_return "program" "read")


(close-output-port out_parse)
(close-output-port out_stream)
(close-output-port out_comment)

;; "program-table('read'):"
;; (program-table "read")
;; "production#-to-list('1'):"
;; (production#-to-list "1")