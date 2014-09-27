; p1
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

; p2
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

; p3
(define (poly_val p x)
    ; recursively evaluate polynomial using Horner's rule
    (if (null? p) 0 ; Base case - p is empty
        (+ (* x (poly_val (cdr p) x)) (car p))
    )
)

(define (poly_list_val p x)
    ; Evaluates the polynomial for every element of x
    (map
        (lambda (b)
            (poly_val p b)
        )
        x
    )
)

(define (poly_multi_val p x)
    ; We can use our previous functions here depending on the situation
    (if (list? x)
        (poly_list_val p x)
        (poly_val p x)
    )
)

(define (poly_add p q)
    (cond
        ; Case p has more elements - append 0 to end of q
        ((> (length p) (length q)) (poly_add p (fold-right cons '(0) q)))
        ; Case q has more elements - append 0 to end of p
        ((< (length p) (length q)) (poly_add (fold-right cons '(0) p) q))
        ; Otherwise p and q same length - add them
        (else (map + p q))
    )
)

(define (poly_scale m p)
    (if (null? p)   ; p is empty - return empty list
        '()
        ; Otherwise - multiply all values in p with m
        (map
            (lambda (x)
                (* m x)
            )
            p
        )
    )
)

(define (poly_mul p q)
    (let
        (
            (a (map
                (lambda (x) (poly_scale x q)) p)
            )
        )
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
