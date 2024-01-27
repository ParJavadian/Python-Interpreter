(define (value-of exp env)
    (case exp
        (statement? (cases exp
            ((assign var expr)(let ([value (value-of expr)])
            (list 
                (empty-var) 
                (if ((var-exists) var env)
                   (update_env var value env)
                   (extend_env var value env)
                )
               )    
            ))
            ()
            ...
        )
        (..?)
    )
    )
)

