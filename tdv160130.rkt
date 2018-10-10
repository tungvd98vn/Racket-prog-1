#lang racket
(provide (all-defined-out))

;; 1. divisible-by-x
(define (divisible-by-x? x)
  (lambda (k)
    (if (= 0 (modulo k x))
        #t
        #f)))

;; 2. function-9
(define (function-9 funct)
  (funct 9))

;; 3. my-map
(define (my-map funct lst)
  (if (null? lst)
      lst
      (cons (funct (car lst)) (my-map funct (cdr lst)))))

;; 4. combine
(define (combine lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      null
      (cons (cons (car lst1) (cons (car lst2) null)) (combine (cdr lst1) (cdr lst2)))))

;; 5. segregate

(define (segregate bool? lst)
  (if (null? lst)
      '(null null)
      (list (true-lst bool? lst) (false-lst bool? lst))))

(define (true-lst bool? lst)
  (if (null? lst)
      null
      (if (bool? (car lst))
          (cons (car lst)(true-lst bool? (cdr lst)))
          (true-lst bool? (cdr lst)))))

(define (false-lst bool? lst)
  (if (null? lst)
      null
      (if (not (bool? (car lst)))
          (cons (car lst)(false-lst bool? (cdr lst)))
          (false-lst bool? (cdr lst)))))

;; 6. is-member?
(define (is-member? lst exp)
  (if (null? exp)
      #f
      (if (equal? lst (car exp))
          #t
          (is-member? lst (cdr exp)))))

;; 7. my-sorted
(define (my-sorted? funct lst)
  (if (<= (length lst) 1)
      #t
      (if (funct (first lst) (second lst))
          (my-sorted? funct (cdr lst))
          #f)))

;; 8. my-flatten
(define (my-flatten lst)
  (if (null? lst)
      null
      (if (list? (car lst))
          (append (my-flatten (car lst))(my-flatten (cdr lst)))
          (append (list (car lst)) (my-flatten (cdr lst))))))

;; 9. upper-threshold
(define (upper-threshold lst key)
  (if (null? lst)
      null
      (if (< (car lst) key)
          (cons (car lst) (upper-threshold (cdr lst) key))
          (upper-threshold (cdr lst) key))))

;; 10. my-lst-ref
(define (my-list-ref lst int)
  (if (null? lst)
      (printf "ERROR: Index out of bound")
      (if (= int 0)
          (car lst)
          (my-list-ref (cdr lst) (- int 1)))))

  
      
