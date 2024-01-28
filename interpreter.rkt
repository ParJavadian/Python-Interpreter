#lang racket
(require "datatypes.rkt")
(require "environment.rkt")
(require "scoping.rkt")
(require (lib "eopl.ss" "eopl"))

(define-datatype return-type return-type?
  (none)
)

(define (interpret-program prog env)
        (begin
            (renew-scope)
            (interpret-program-block prog (add-scope (init-scope)))
        ))

(define (get-env scope-index)
(scope->env (get-scope scope-index)))

(define (interpret-program-block pb scope-index)
    (if (null? pb)
        (get-env scope-index)
        (let ([return-val (value-of (car pb) scope-index)])
        (cases return-type return-val
        (none (interpret-program-block (cdr pb) scope-index))))))

(define (value-of exp scope-index)
    (cond
        ((statement? exp) (cases statement exp
            (assign (var expr)
            (let ([index (if (is-global? var scope-index)
                            0
                            scope-index)])

                        (extend-scope index var (value-of expr scope-index))
                        (none)
            ))
            (return (expr)  (let ([value (car (value-of expr env))])
                            (list value env))
                        )
            (return_void null)
            (pass null)
             )

            (else (none))
        )
        ;;; (..?)
    )
        (else none)
    )
)

(provide (all-defined-out))