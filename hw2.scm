; (let ((p1 v1) ... (pn vn)) body)
; ((lambda (p1 ... pn) body) v1 ... vn)
(define (rewrite-let expr)
    (append
        (list
            (append
                (list 'lambda               ; lambda
                    (map car (cadr expr))   ; (p1 ... pn)
                )
                    (cdr (cdr expr))        ; body
            )
        )
        (map cadr (cadr expr)))             ; v1 ... vn
)

(define (rewrite-cond expr)
    (if (equal? 'else (car (car (cdr expr)))) (cadr (car (cdr expr)))   ; case (else z) => return z
        (let
            (
                (first (car (car (cdr expr))))                          ; example: a from (a b)
                (second (cadr (car (cdr expr))))                        ; example: b from (a b)
            )
            (append '(if)                                               ; put together (if a b ( .... )) recursively
                (list first second
                (rewrite-cond (remove (list first second) expr))))
        )
    )
)

(define (poly_val p x)
    (if (null? p) 0                                 ; recursively evaluate polynomial
        (+ (* x (poly_val (cdr p) x)) (car p))
    )
)

(define (poly_list_val p x)
    (map
        (lambda (b)
            (if (null? p) 0
                (+
                    (*
                        b
                        (poly_val (cdr p) b)
                    )
                    (car p)
                )
            )
        )
        x
    )
)

(define (poly_multi_val p x)
    (if (list? x)
        (poly_list_val p x)
        (poly_val p x)
    )
)

(define (poly_add p q)
    (cond
        ((> (length p) (length q)) (poly_add p (fold-right cons '(0) q)))
        ((< (length p) (length q)) (poly_add (fold-right cons '(0) p) q))
        (else (map + p q))
    )
)

(define (poly_scale m p)
    (if (null? p) '()
        (map
            (lambda (x)
                (* m x)
            )
            p
        )
    )
)

(define (poly_mul p q)
    (let ((a (map
        (lambda (x)
            (poly_scale x q)
        )
        p
    )))
        (cond
            ((null? a) '())
            (else (cdr (poly_mul_helper a)))
        )
    )
)

(define (poly_mul_helper a)
    (cond
        ((null? (cdr a)) (cons 0 (car a)))
        (else (cons 0 (poly_add (car a) (poly_mul_helper (cdr a)))))
    )
)
