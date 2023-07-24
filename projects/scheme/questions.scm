(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement



;; Problem 15
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 15
  (define (helper x y) (cond 
                        ((null? x) nil)
                        (else (append (list (list y (car x))) (helper (cdr x) (+ y 1))))))
  (helper s 0)
  )
  ; END PROBLEM 15

;; Problem 16

;; Merge two lists LIST1 and LIST2 according to COMP and return
;; the merged lists.
(define (merge comp list1 list2)
  ; BEGIN PROBLEM 16
  (cond
    ((and (null? list1) (null? list2)) nil)
    ((null? list1) list2)
    ((null? list2) list1)
    ((comp (car list1) (car list2)) (append (list (car list1) (car list2) ) (merge comp (cdr list1) (cdr list2))))
    (else (append (list (car list2) (car list1) ) (merge comp (cdr list1) (cdr list2)))))
  )
  ; END PROBLEM 16


(merge < '(1 5 7 9) '(4 8 10))
; expect (1 4 5 7 8 9 10)
(merge > '(9 7 5 1) '(10 8 4 3))
; expect (10 9 8 7 5 4 3 1)

(define (zip pairs)
  (define (helper p x y)(cond
                          ((null? p) (append (list x) (list y)))
                          (else (helper (cdr p) (append x (list (caar p))) (append y (list (car (cdar p))))))
                          ))
  (helper pairs () ())
  )

;; Problem 17

(define (nondecreaselist s)
    ; BEGIN PROBLEM 17
    (define (helper x y m) (cond
                           ((null? x) (list m))
                           ((< (car x) y) (append (list m) (helper x (car x) nil)))
                           (else (helper (cdr x) (car x) (append m (list (car x)))))))
    (helper s (car s) nil)
    ; END PROBLEM 17
    )

;; Problem EC
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond 
        ((atom? expr)
         ; BEGIN PROBLEM EC
         expr
         ; END PROBLEM EC
         )
        ((quoted? expr)
         ; BEGIN PROBLEM EC
         expr
         ; END PROBLEM EC
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM EC
           ; (if (null? (cdr body))
           ;   (append (list form) (list params) (list (let-to-lambda (car body))))
           ;   (append (list form) (list params) (list (let-to-lambda (car body))) (list (let-to-lambda (cdr body)))))
           (cons form (cons params (let-to-lambda body)))
           ; END PROBLEM EC
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM EC
             ; (cond
             ;   ((or (not (pair? values)) (not (pair? body))) expr)
             ;   (else (append (list (list 'lambda (car (zip values)) (let-to-lambda (car body)))) (let-to-lambda (cdr (zip values))))))
            (cons (cons 'lambda (cons (let-to-lambda (car (zip values))) (cons (let-to-lambda (car body)) nil))) (map let-to-lambda (cadr (zip values))))
           ; END PROBLEM EC
           ))
        (else
         ; BEGIN PROBLEM EC
         ; (cond
         ;   ((null? expr) nil)
         ;   (else (cond 
         ;           ((list? (car expr)) (append (let-to-lambda (car expr)) (let-to-lambda (cdr expr))))
         ;           (else (append (list (car expr)) (let-to-lambda (cdr expr)))))))
         (cons (car expr) (map let-to-lambda (cdr expr)))
         ; END PROBLEM EC
         )))
