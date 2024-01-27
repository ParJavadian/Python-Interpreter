#lang racket
(require "datatypes.rkt")
(require "environment.rkt")
(require (lib "eopl.ss" "eopl"))

(define (value-of exp env)
    (cond
        ((statement? exp) (cases statement exp
            (assign (var expr) (let ([value (car (value-of expr env))])
            (list 
                value
                (if (var-exists var env)
                   (update_env var value env)
                   (extend_env var value env)
                )
               )    
            ))
            ;;; ()
            ;;; ...
            (list 1 3)
        )
        ;;; (..?)
    )
        (else (list 4 5))
    )
)

(provide (all-defined-out))