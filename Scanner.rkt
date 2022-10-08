#lang racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

;;The following function is the Scanner. This function will take in a text file, and return a list of "tokens" based on the grammar below.
;;These "tokens" will each be a list comprised of a type, line number, and the literal data they came from.
;;For example ('id 1 "a")

;Grammar:
;assign → :=
;plus   → +
;minus  → -
;times  → *
;div    → /
;lparen → (
;rparen → )
;id     → letter (letter | digit)*
;number → digit digit* | digit* (. digit | digit .) digit*

(define (lex_index input-port [line_num 1])
  (define lex
    (lexer
     ;skip spaces:
     [#\space    (lex_index input-port line_num)]
     ;skip newline (the line number is effectively incremented each time a new line is encoutered)
     [#\newline  (lex_index input-port (+ line_num 1))] 
     ;skip newline (the line number is effectively incremented each time a new line is encoutered)
     ["\r\n"     (lex_index input-port (+ line_num 1))]
     ;end of file
     ["$$"       (list (list 'eof        line_num lexeme))]
     ;assign opp
     [":="       (cons (list 'assign_opp line_num lexeme) (lex_index input-port line_num))]
     ;addition opp
     ["+"        (cons (list 'add_opp    line_num lexeme) (lex_index input-port line_num))]
     ;subtraction opp
     ["-"        (cons (list 'sub_opp    line_num lexeme) (lex_index input-port line_num))]
     ;multi opp
     ["*"        (cons (list 'multi_opp  line_num lexeme) (lex_index input-port line_num))]
     ;division opp
     ["/"        (cons (list 'div_opp    line_num lexeme) (lex_index input-port line_num))]
     ;left paren
     ["("        (cons (list 'l_paren    line_num lexeme) (lex_index input-port line_num))]
     ;right paren
     [")"        (cons (list 'r_paren    line_num lexeme) (lex_index input-port line_num))]
     ;Read statement
     ["read"     (cons (list 'READ       line_num lexeme) (lex_index input-port line_num))]
     ;Write statement
     ["write"    (cons (list 'WRITE      line_num lexeme) (lex_index input-port line_num))]
     ;id - letter ( letter | digit ) except for read / write
     [(:: alphabetic (:* (:or numeric alphabetic))) (cons (list 'id line_num lexeme) (lex_index input-port line_num))]
     ;number - digit digit * | digit * ( . digit | digit . ) digit *
     [(:or (:: numeric (:* numeric)) (:: "." (:* numeric)) (:: numeric (:* (:or (:: "." (:* numeric)) (:* numeric (:* "." (:* numeric))))))) (cons (list 'number line_num lexeme) (lex_index input-port line_num))]
     ))
  (lex input-port))

(provide lex_index)