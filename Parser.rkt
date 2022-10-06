#lang racket
(require "Scanner.rkt")

;The Grammar
;-----------------------------------------------
;program -> stmt_list $$
;stmt_list -> stmt stmt_list | ε
;stmt -> id := expr | read id | write expr
;expr -> term term_tail
;term_tail -> add_opp term term_tail | ε
;term -> factor factor_tail
;factor_tail -> mult_op factor factor_tail | ε
;factor -> ( expr ) | id | number
;add_op -> + | -
;mult_op -> * | /
;-----------------------------------------------

(define (parse file_name)
  (define file (open-input-file file_name))
  (define token_list (lex_index file))
  (if (empty? (program token_list))
      (display "Accept")                    ;If the last list returned is empty, then we know the parse was successful.
      (display "Parse error")               ;<- This really should never happen, but if it does, theres a case for it. 
  ))

(define (match type token_list)
  (if (equal? (first (first token_list)) type)
      (rest token_list)
      (error (format "Syntax error on line ~a" (second (first token_list))))))

(define (program token_list)
  (case (first (first token_list))
    [(id READ WRITE eof) (match 'eof (stmt_list token_list))] 
    [else (error (format "Syntax error on line ~a" (second (first token_list))))]))

(define (stmt_list token_list)
  (case (first (first token_list))
    [(id READ WRITE) (stmt_list (stmt token_list))]
    [(eof) token_list]
    [else (error (format "Syntax error on line ~a" (second (first token_list))))]))

(define (stmt token_list)
  (case (first (first token_list))
    [(id) (expr (match 'assign_opp (match 'id token_list)))]
    [(READ) (match 'id (match 'READ token_list))]
    [(WRITE) (expr (match 'WRITE token_list))]
    [else (error (format "Syntax error on line ~a" (second (first token_list))))]))

(define (expr token_list)
  (case (first (first token_list))
    [(id number l_paren) (term_tail (term token_list))]
    [else (error (format "Syntax error on line ~a" (second (first token_list))))]))

(define (term_tail token_list)
  (case (first (first token_list))
    [(add_opp sub_opp) (term_tail (term (add_op token_list)))]
    [(r_paren id READ WRITE eof) token_list]
    [else (error (format "Syntax error on line ~a" (second (first token_list))))]))

(define (term token_list)
  (case (first (first token_list))
    [(id number l_paren) (factor_tail (factor token_list))]
    [else (error (format "Syntax error on line ~a" (second (first token_list))))]))

(define (factor_tail token_list)
  (case (first (first token_list))
    [(multi_opp div_opp) (factor_tail (factor (mult_op token_list)))]
    [(add_opp sub_opp r_paren id READ WRITE eof) token_list]
    [else (error (format "Syntax error on line ~a" (second (first token_list))))]))

(define (factor token_list)
  (case (first (first token_list))
    [(id) (match 'id token_list)]
    [(number) (match 'number token_list)]
    [(l_paren) (match 'r_paren (expr (match 'l_paren token_list)))]
    [else (error (format "Syntax error on line ~a" (second (first token_list))))]))

(define (add_op token_list)
  (case (first (first token_list))
    [(add_opp) (match 'add_opp token_list)]
    [(sub_opp) (match 'sub_opp token_list)]
    [else (error (format "Syntax error on line ~a" (second (first token_list))))]))

(define (mult_op token_list)
  (case (first (first token_list))
    [(multi_opp) (match 'multi_opp token_list)]
    [(div_opp) (match 'div_opp token_list)]
    [else (error (format "Syntax error on line ~a" (second (first token_list))))]))
    
;(parse "InputFiles/input01.txt")
(provide parse)
