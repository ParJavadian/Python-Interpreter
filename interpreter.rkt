#lang racket
(require "datatypes.rkt")
(require "environment.rkt")
(require "scoping.rkt")
(require (lib "eopl.ss" "eopl"))

(define-datatype return-type return-type?
  (none)
  (val (value (lambda (x) #t)))
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

(define (expressions->vals expressions-in scope-index)
    (cases expression* expressions-in
        (empty-expr () null)
        (expressions (expr rest-exprs)
            (append (expressions->vals rest-exprs scope-index) (list (value-of expr scope-index))))
        ))

(define (print-all vals)
    (if (null? vals)
        vals
        (begin
         (cond 
            [(eq? #t (car vals)) (display "True")]
            [(eq? #f (car vals)) (display "False")]
            [else (display(car vals))])
         (display "\n")
         (print-all (cdr vals)))))

(define (handle-loop i lst body scope-index)
    (cond
        ((empty? lst) (none))
        (else (begin
            (extend-scope scope-index i (car lst))
            (let ([body-val (value-of body scope-index)])
             (cond
                ((control-signal? body-val)
                    (cases control-signal body-val
                        (break-signal () null)
                        (else (handle-loop i (cdr lst) body scope-index))
                ))
                (else (handle-loop i (cdr lst) body scope-index)))
             )
             )
        )
    )
)


(define (value-of exp scope-index)
    (cond
        ((statement? exp) (cases statement exp
            (assign (var expr)
                (let ([index (if (is-global? var scope-index)
                                0
                                scope-index)])

                            (extend-scope index var (value-of expr scope-index))
                            (none)))
            ;;; TODO : maybe debug needed
            (global (var) 
                (begin 
                    (extend-scope-globals scope-index var)
                    (none)))
            (return (expr)  (let ([value (car (value-of expr scope-index))])
                                (val value)))
            (return_void () (none))
            (pass () (none))
            (print_stmt (expressions) 
                (let ([vals (expressions->vals expressions scope-index)])
                    (begin
                     (print-all vals)
                     (none))))
            (break () (break-signal))
            (continue () (continue-signal))
            (for_stmt (i list_exp sts)
                (let ([iterating-list (value-of list_exp scope-index)])
                    (handle-loop i iterating-list sts scope-index)
                )
            )
            (else (none))
        ))
        ((expression? exp) (cases expression exp
            (binary_op (op left right) (op (value-of left scope-index) (value-of right scope-index)))
            (unary_op (op operand) (op (value-of operand scope-index)))
            (ref (var) (apply-scope scope-index var))
            (list_ref (ref index) (list-ref (value-of ref scope-index) (value-of index scope-index)))
            (atomic_num_exp (num) num)
            (atomic_bool_exp (bool) bool)
            (atomic_null_exp () (none))
            (atomic_list_exp (lst) (
                expressions->vals lst scope-index
            ))
            (else 0)))
        (else none)
    )
)

(provide (all-defined-out))