#lang racket

(require "passes/parser.rkt")
(require "passes/lexer.rkt")
(require "interpreter.rkt")
(require "environment.rkt")
(require "datatypes.rkt")


(define (parse-scan prog-string)
  (python-parser (lex-this prog-string))
  )

(define (evaluate file-name)
  (interpret-program (parse-scan (string-join (file->lines file-name))) (empty_env))
  )

(provide (all-defined-out))

;(evaluate "test_easy.py")
;(parse-scan (string-join (file->lines "test_easy.py")))
;(value-of (evaluate "test.py") (empty_env))
;(value-of (assign "i" (atomic_num_exp 2)) (empty_env))