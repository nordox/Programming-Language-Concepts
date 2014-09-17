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
