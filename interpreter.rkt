#lang racket
(require "datatypes.rkt")
(require "environment.rkt")
(require (lib "eopl.ss" "eopl"))

(define (interpret-program prog env)
    (if (null? prog)
    env
    (let ((new-env (cadr (value-of (car prog) env))))
    (interpret-program (cdr prog) new-env))))

(define (value-of exp env)
    (cond
        ((statement? exp) (cases statement exp
            (assign (var expr) (let ([value (car (value-of expr env))])
            (list 
                value
                (update_env var value env)
               )    
            ))
            (list 1 env)
        )
        ;;; (..?)
    )
        (else (list 4 env))
    )
)

(provide (all-defined-out))