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
    (if (equal? 'else (car (car (cdr expr)))) (cadr (car (cdr expr)))
        (let
            (
                (first (car (car (cdr expr))))
                (second (cadr (car (cdr expr))))
            )
            (append '(if)
                (list first second
                (test (remove (list first second) expr))))
        )
    )
)

(define (poly_val p x)
    (if (null? p) 0
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
    (poly_list_val p x)
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
    (map
        (lambda (x)
            (poly_scale x q)
        )
        p
    )
)

(define (try-rewrite-let expr)
  (display "in  : ") (display expr) (newline)
  (display "out : ") (display (rewrite-let expr)) (newline)
  (newline)
)

(define (try-rewrite-cond expr)
  (display "in  : ") (display expr) (newline)
  (display "out : ") (display (rewrite-cond expr)) (newline)
  (newline)
)

(define (test_val p x)
    (display p) (display " at ") (display x) (display " = ") (display (poly_val p x)) (newline)
)

(define (test_list_val p x)
    (display p) (display " at ") (display x) (display " = ") (display (poly_list_val p x)) (newline)
)

(define (test_multi_val p x)
    (display p) (display " at ") (display x) (display " = ") (display (poly_multi_val p x)) (newline)
)

(define (test_add p q)
    (display p) (display " + ") (display q) (display " = ") (display (poly_add p q)) (newline)
)

(define (test_scale m p)
    (display m) (display " * ") (display p) (display " = ") (display (poly_scale m p)) (newline)
)

(define (test_mul p q)
    (display p) (display " * ") (display q) (display " = ") (display (poly_mul p q)) (newline)
)
