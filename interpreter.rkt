#lang racket
(require "datatypes.rkt")
(require "environment.rkt")
(require "scoping.rkt")
(require "thunkk.rkt")
(require (lib "eopl.ss" "eopl"))

(define-datatype return-type return-type?
  (none)
  (val (value (lambda (x) #t)))
  (break-signal)
  (continue-signal)
)

(define (interpret-program prog env)
    (begin
        (renew-scope)
        (interpret-program-block prog (add-scope (init-scope)))
    )
)

(define (get-env scope-index)
    (scope->env (get-scope scope-index))
)

(define (interpret-program-block pb scope-index)
    (cond
        ((null? pb) (none))
        (else (let ([return-val (value-of (car pb) scope-index)])
            (cases return-type return-val
                (none () (interpret-program-block (cdr pb) scope-index))
                (val (value) value)
                (break-signal () (begin 
                (break-signal)))
                (continue-signal () (continue-signal))
        )))
    )
)

(define (expressions->vals expressions-in scope-index)
    (cases expression* expressions-in
        (empty-expr () null)
        (expressions (expr rest-exprs)
            (append (expressions->vals rest-exprs scope-index) (list (value-of expr scope-index))))
    )
)


(define (list-copy list)
  (if (null? list) '() (cons (car list) (list-copy (cdr list))))
)

(define (value-of-thunkk th)
    (cases thunkk th
        (a-thunkk (expr scopes-state scope-index)
            (let ([temp-scopes (list-copy scopes)])
                  (begin
                    (set-scopes scopes-state)
                    (let ([value (value-of expr scope-index)])
                        (set-scopes temp-scopes)
                        value
                  )
                )
            )
        )
    )
)

(define (print-all vals)
    (if (null? vals)
        vals
        (begin
         (cond 
            [(eq? #t (car vals)) (display "True")]
            [(eq? #f (car vals)) (display "False")]
            [else (display(car vals))])
         (display "\n")
         (print-all (cdr vals)))
    )
)

(define (handle-loop i lst body scope-index)
    (cond
        ((empty? lst) (none))
        (else (begin
            (extend-scope scope-index i (car lst))
            (let ([body-val (interpret-program-block body scope-index)])
             (cond
                ((return-type? body-val)
                    (cases return-type body-val
                        (break-signal () (none))
                        (else (handle-loop i (cdr lst) body scope-index))
                ))
                (else (handle-loop i (cdr lst) body scope-index))))
             )
        )
    )
)

(define handle-if 
    (lambda (condition if_sts else_sts scope-index)
        (cond 
            ((and (eq? condition #t) (not(eq? condition 0))) (interpret-program-block if_sts scope-index))
            (else (interpret-program-block else_sts scope-index))
        )
    )
)

(define (handle-binary-op op left-val right-th)
    (if (equal? `* (object-name op))
     (if (zero? left-val) 0 (* left-val (value-of-thunkk right-th)))
     (op left-val (value-of-thunkk right-th))
    )
)

(define (params-list params scope-index)
    (cases func_param* params
        (empty-param () (empty-eval-func-param))
        (func_params (param rest-params)
            (eval-func-params 
                (cases func_param param
                    (with_default (var expr)
                        (eval_with_default var (a-thunkk expr scopes scope-index))))
                (params-list rest-params scope-index)))
    )
)

(define (extend-scope-with-params func-params in-params scope-index calling-scope-index)
    (cases expression* in-params
        (empty-expr ()
            (cases eval-func-param* func-params
                (empty-eval-func-param () (none))
                (eval-func-params (eval-param rest-evals)
                    (cases eval-func-param eval-param
                        (eval_with_default (var val)
                            (begin
                                (extend-scope scope-index var val)
                                (extend-scope-with-params rest-evals in-params scope-index calling-scope-index)
                                (none)
                            ))))))
        (expressions (expr rest-exprs)
            (cases eval-func-param* func-params
                (empty-eval-func-param () (none))
                (eval-func-params (eval-param rest-evals)
                    (cases eval-func-param eval-param
                        (eval_with_default (var val)
                            (begin
                                (extend-scope scope-index var (a-thunkk expr scopes scope-index))
                                (extend-scope-with-params rest-evals rest-exprs scope-index calling-scope-index)
                                (none)
                            ))))))
    )
)

(define (handle-func-call func-name in-params scope-index)
    (let  ([func (value-of func-name scope-index)])
        (cases proc func
            (new-proc (params statements parent-scope)
                (let ([new-scope (add-scope (child-scope parent-scope))])
                        (begin
                            (if (and (thunkk? in-params) (thunkk? params))
                                (extend-scope-with-params (value-of-thunkk params) (value-of-thunkk in-params) new-scope scope-index)
                                (if (thunkk? in-params)
                                    (extend-scope-with-params params (value-of-thunkk in-params) new-scope scope-index)
                                    (if (thunkk? params)
                                        (extend-scope-with-params (value-of-thunkk params) in-params new-scope scope-index)
                                        (extend-scope-with-params params in-params new-scope scope-index))))
                            (interpret-program-block statements new-scope))))))
)

(define (value-of exp scope-index)
    (cond
        ((statement? exp) (cases statement exp
            (assign (var expr)
                (let ([index (if (is-global? var scope-index)
                                0
                                scope-index)])

                            (extend-scope index var (a-thunkk expr scopes scope-index))
                            (none)))
            (global (var) 
                (begin 
                    (extend-scope-globals scope-index var)
                    (none)))
            (return (expr)  (let ([value (value-of expr scope-index)])
                                (val value)))
            (return_void () (none))
            (pass () (none))
            (func (name params statements)
                (begin
                    (extend-scope scope-index name
                        (new-proc (params-list params scope-index) statements scope-index))
                    (none)))
            (print_stmt (expressions) 
                (let ([vals (expressions->vals expressions scope-index)])
                    (begin
                     (print-all vals)
                     (none))))
            (break () (break-signal))
            (continue () (continue-signal))
            (for_stmt (i list_exp sts)
                (let ([iterating-list (value-of list_exp scope-index)] [new-scope (add-scope (child-scope scope-index))])
                    (handle-loop i iterating-list sts new-scope)
                )
            )
            (if_stmt (cond_exp if_sts else_sts)
                    (let ([condition (value-of cond_exp scope-index)])
                        (handle-if condition if_sts else_sts scope-index))
            )
            (else (none))
        ))
        ((expression? exp) (cases expression exp
            (binary_op (op left right) (handle-binary-op op (value-of left scope-index) (a-thunkk right scopes scope-index)))
            (unary_op (op operand) (op (value-of operand scope-index)))
            (function_call (func params)
                (handle-func-call func params scope-index))
            (ref (var) (let ([saved-val (apply-scope scope-index var)])
                (if (thunkk? saved-val)  (value-of-thunkk saved-val) saved-val)
            ))
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