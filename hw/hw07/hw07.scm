(define (filter-lst fn lst)
  (cond
    ((null? lst) ())
    (else (if (fn (car lst)) (cons (car lst) (filter-lst fn (cdr lst))) (filter-lst fn (cdr lst)))))
)

;;; Tests
(define (even? x)
  (= (modulo x 2) 0))
(filter-lst even? '(0 1 1 2 3 5 8))
; expect (0 2 8)


(define (interleave first second)
  (cond
    ((and (null? first) (null? second)) ())
    ((null? first) (cons (car second) (interleave first (cdr second))))
    ((null? second) (cons (car first) (interleave (cdr first) second)))
    (else (cons (car first) (cons (car second) (interleave (cdr first) (cdr second))))))
)

(interleave (list 1 3 5) (list 2 4 6))
; expect (1 2 3 4 5 6)

(interleave (list 1 3 5) nil)
; expect (1 3 5)

(interleave (list 1 3 5) (list 2 4))
; expect (1 2 3 4 5)


(define (accumulate combiner start n term)
  (cond
    ((= start 0) (cond 
                   ((= n 1) (term n))
                   (else (combiner (term n) (accumulate combiner 0 (- n 1) term)))))
    (else (cond 
            ((= n 1) (combiner start (term n)))
            (else (combiner start (combiner (term n) (accumulate combiner 0 (- n 1) term)))))))
)


(define (no-repeats lst)
  (cond 
    ((null? lst) ())
    (else (cons (car lst) (no-repeats (filter (lambda (x) (not (= (car lst) x))) (cdr lst))))))
)

