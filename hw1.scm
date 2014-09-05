; CMPSC 461, Homework 1
; Nicholas Krzenski, ngk5036@psu.edu

; fibonacci function (two recursive calls)
(define (p1 n)
    (cond 
        ((= n 0) 1)
        ((= n 1) 1)
        (else (+ (p1 (- n 1)) (p1 (- n 2)))))
)

; fibonacci function (two recursive calls)
(define (p2 n)
    (cond 
        ((= n 0) 1)
        ((= n 1) 1)
        (else (p2_helper 2 1 1 n)))
)

; helper function
(define (p2_helper a b count n)
    (if (= count n)
        b
        (p2_helper (+ a b) a (+ count 1) n)
    )
)

; generalized fibonacci
(define (p3 n base-case coefficients)
    ; base cases
    (cond
        ((= n 0) (car base-case))
        ((= n 1) (cadr base-case))
        (else (+ (* (car coefficients) (p3 (- n 1) base-case coefficients)) (* (cadr base-case) (p3 (- n 2) base-case coefficients))))
    )
)
; bits in a bucket
(define (p4 m n)
  ; base case - last bit?
  ;(display m) (display "\t") (display n) (newline)
  (cond
    ((and (= m 0) (not(= n 0))) 1)
    ((= n 1) 1)
    ((= m 1) 0)
    (else
      ; check which bits were selected and
      ; decide which bit to add back
      ((lambda (temp) 
         ;(display temp) (display "\t")
         (cond
           ((equal? temp (cons 0 0)) (p4 (- m 2) (+ n 1)))
           ((equal? temp (cons 1 1)) (p4 m (- n 1)))
           (else (p4 m (- n 1)))))
       (cons (random 2) (random 2)))
      )
    )
  )

; test function
(begin
  (define nums '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
  (display "p1 ") (display (map p1 nums)) (newline)
  (display "p2 ") (display (map p2 nums)) (newline)
  (display "p3 ") (display (map (lambda (n) (p3 n '(1 1) '(1 1))) nums)) (newline)
  (newline)
)
