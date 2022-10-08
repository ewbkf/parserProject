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

;;Parse
(define (parse file_name)
  (define file (open-input-file file_name))
  (define token_list (lex_index file))
  (if (empty? (program token_list))
      (display "Accept")                                                            ;If the last list returned is empty, then we know the parse was successful.
      (display "Warning: Not all characters/tokens were consumed.")                 ;<- This really should never happen, but if it does, theres a case for it. 
  ))

;;Match
(define (match type token_list)                                                     ;Match takes in the list, extracs the first token in the list, then extracts the type from the token.
  (if (equal? (first (first token_list)) type)                                      ;If the type extracted matches the type expected, the token is "consumed" and the rest of the list 
      (rest token_list)                                                             ;is returned.
      (error (format "Syntax error on line ~a" (second (first token_list))))))

;;The below functions will call eachother recursively until a terminal is reached. Once a terminal is reached, that token is checked and consumed.
;;This is repeated until either all tokens are consumed and an empty list is returned, or a syntax error is found. 

;Program
(define (program token_list)
  (case (first (first token_list))
    [(id READ WRITE eof) (match 'eof (stmt_list token_list))] 
    [else (error (format "Syntax error on line ~a" (second (first token_list))))]))

;;Statement List
(define (stmt_list token_list)
  (case (first (first token_list))
    [(id READ WRITE) (stmt_list (stmt token_list))]
    [(eof) token_list]
    [else (error (format "Syntax error on line ~a" (second (first token_list))))]))

;;Statement
(define (stmt token_list)
  (case (first (first token_list))
    [(id) (expr (match 'assign_opp (match 'id token_list)))]
    [(READ) (match 'id (match 'READ token_list))]
    [(WRITE) (expr (match 'WRITE token_list))]
    [else (error (format "Syntax error on line ~a" (second (first token_list))))]))

;;Expresion
(define (expr token_list)
  (case (first (first token_list))
    [(id number l_paren) (term_tail (term token_list))]
    [else (error (format "Syntax error on line ~a" (second (first token_list))))]))

;;Term Tail
(define (term_tail token_list)
  (case (first (first token_list))
    [(add_opp sub_opp) (term_tail (term (add_op token_list)))]
    [(r_paren id READ WRITE eof) token_list]
    [else (error (format "Syntax error on line ~a" (second (first token_list))))]))

;;Term
(define (term token_list)
  (case (first (first token_list))
    [(id number l_paren) (factor_tail (factor token_list))]
    [else (error (format "Syntax error on line ~a" (second (first token_list))))]))

;;Factor Tail
(define (factor_tail token_list)
  (case (first (first token_list))
    [(multi_opp div_opp) (factor_tail (factor (mult_op token_list)))]
    [(add_opp sub_opp r_paren id READ WRITE eof) token_list]
    [else (error (format "Syntax error on line ~a" (second (first token_list))))]))

;;Factor
(define (factor token_list)
  (case (first (first token_list))
    [(id) (match 'id token_list)]
    [(number) (match 'number token_list)]
    [(l_paren) (match 'r_paren (expr (match 'l_paren token_list)))]
    [else (error (format "Syntax error on line ~a" (second (first token_list))))]))

;;Addition Opperator
(define (add_op token_list)
  (case (first (first token_list))
    [(add_opp) (match 'add_opp token_list)]
    [(sub_opp) (match 'sub_opp token_list)]
    [else (error (format "Syntax error on line ~a" (second (first token_list))))]))

;;Multiplication Opperator
(define (mult_op token_list)
  (case (first (first token_list))
    [(multi_opp) (match 'multi_opp token_list)]
    [(div_opp) (match 'div_opp token_list)]
    [else (error (format "Syntax error on line ~a" (second (first token_list))))]))
    
(provide parse)
