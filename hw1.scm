; CMPSC 461, Homework 1
; Nicholas Krzenski
; ngk5036@psu.edu

; fibonacci function (two recursive calls)
(define (p1 n)
    (cond 
        ((= n 0) 1)
        ((= n 1) 1)
        (else (+ (p1 (- n 1)) (p1 (- n 2))))
    )
)

; fibonacci function (one recursive call using helper function)
(define (p2 n)
    (cond 
        ((= n 0) 1)
        ((= n 1) 1)
        (else (p2_helper 2 1 1 n))
    )
)

; helper function for p2
(define (p2_helper a b count n)
    (if (= count n)
        b
        (p2_helper (+ a b) a (+ count 1) n)
    )
)

; generalized fibonacci
(define (p3 n base-case coefficients)
    (let    ; define base cases as f0 and f1 and coefficients as a and b
        (
          (f0   (car base-case))
          (f1   (cadr base-case))
          (a    (car coefficients))
          (b    (cadr coefficients))
        )
        (cond
            ((= n 0) f0)
            ((= n 1) f1)
            (else (+ (* a (p3 (- n 1) base-case coefficients)) (* b (p3 (- n 2) base-case coefficients))))
        )
    )
)

; bits in a bucket
(define (p4 m n)
    ; case even # of 0 bits => 1
    ; case odd # of 0 bits  => 0
    (cond   ; first make sure data is valid
        ((and (= m 0) (= n 0)) -1)
        ((or (< m 0) (< n 0)) -1)
        ((even? m) 1)
        (else 0)
    )
)
