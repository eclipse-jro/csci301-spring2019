; 1) program        --> stmt_list $$
; 2) stmt_list      --> stmt stmt_list
; 3) smt_list       --> e
; 4) stmt           --> id := expr
; 5) stmt           --> read id

; inputStackTokens (read, A, read, B, C, :=, A, +, B, D)
; parseStack = (program)

(define program-table
  '(1 0 1 1 0 0 0 0 0 0 0 1))
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

(cons (cons program-table stmt_list-table) stmt-table)
