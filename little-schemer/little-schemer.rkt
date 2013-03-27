#lang racket

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

; zero? add1 sub1 already defined in racket!

(define (lat? l)
  (cond [(null? l) #t]
        [(atom? (car l)) (lat? (cdr l))]
        [#t #f]))
 
; null? eq? already defined in racket.

(define (member? a lat)
  (cond [(null? lat) #f]
        [#t (or (eq? (car lat) a)
                (member? a (cdr lat)))]))

; Little Schemer - Chapter 3

(define (rember a lat)
  (cond [(null? lat) (quote())]
        [(eq? (car lat) a) (cdr lat)]
        [#t (cons (car lat) (rember a (cdr lat)))]))

(define (firsts l)
  (cond [(null? l) (quote())]
        [#t (cons (caar l) (firsts (cdr l)))]))

(define (seconds l)
  (cond [(null? l) (quote())]
        [#t (cons (cadar l) (seconds (cdr l)))]))

(define (insertR new old lat)
  (cond [(null? lat) (quote())]
        [(eq? (car lat) old) (cons old (cons new (cdr lat)))]
        [#t (cons (car lat) (insertR new old (cdr lat)))]))

(define (insertL new old lat)
  (cond [(null? lat) (quote())]
        [(eq? (car lat) old) (cons new (cons old (cdr lat)))]
        [#t (cons (car lat) (insertL new old (cdr lat)))]))