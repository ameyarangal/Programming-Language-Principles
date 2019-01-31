#lang racket

(define list-ref
  (lambda (ls n)
    (letrec
      ((nth-cdr
         (lambda (n)
	   ;; complete the definition
           (cond
             [(zero? n) ls]
             [else (cdr (nth-cdr (sub1 n)))])
           )))
      (car (nth-cdr n)))))

(define union
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) ls2]
      [(if (memv (car ls1) ls2)(union (cdr ls1) ls2) (cons (car ls1) (union (cdr ls1) ls2)))])
    ))

(define extend
  (lambda (x p)
    (lambda (y)
      (or (eqv? x y) (p y)))))

(define find-symbol
  (lambda (s als)
    (cond
      [(null? als) #f]
      [(eqv? (car (car als)) s) (cdr (car als))]
      [else (find-symbol s (cdr als))])))

(define walk-symbol
  (lambda (s als)
    (cond
      [(null? als) s]
      [else (let ([x (find-symbol s als)])
              (if x
                  (if (symbol? x)
                      (walk-symbol x als)
                      x)
                  s))])))


(define lambda->lumbda
  (lambda (e)
    (match e
      [(? symbol? e) e]
      [`(lambda (,x) ,body) `(lumbda(,x) ,(lambda->lumbda body))]
      [`(,rator ,rand) `( ,(lambda->lumbda rator) ,(lambda->lumbda rand))])))

(define var-occurs?
  (lambda (var expr)
    (match expr
      [(? symbol? expr) (eqv? var expr)]
      [`(lambda (,x) ,body) (var-occurs? var body)]
      [`(,rator ,rand) (or
                        (var-occurs? var rator)
                        (var-occurs? var rand))])))

(define vars
  (lambda (expr)
    (match expr
      [(? symbol? expr) (cons expr '())]
      [`(lambda (,x) ,body) (vars body)]
      [`(,rator ,rand) (append
                        (vars rator)
                        (vars rand))])))

(define unique-vars
  (lambda (expr)
    (match expr
      [(? symbol? expr) (list expr)]
      [`(lambda (,x) ,body) (unique-vars body)]
      [`(,rator ,rand) (union
                        (unique-vars rator)
                        (unique-vars rand))])))

(define var-occurs-free?
  (lambda (var expr)
    (match expr
      [(? symbol? expr) (eqv? var expr)]
      [`(lambda (,x) ,body) (if (eqv? var x)
                           #f
                           (var-occurs-free? var body))]
      [`(,rator ,rand) (or
                        (var-occurs-free? var rator)
                        (var-occurs-free? var rand))])))

(define var-occurs-bound?
  (lambda (var expr)
    (match expr
      [(? symbol? expr) #f]
      [`(lambda (,x) ,body) (or
                        (and (eqv? var x) (var-occurs? var body)) 
                        (var-occurs-bound? var body))]
      [`(,rator ,rand) (or
                        (var-occurs-bound? var rator)
                        (var-occurs-bound? var rand))])))

(define unique-free-vars
  (lambda (expr)
    (match expr
      [(? symbol? expr) (list expr)]
      [`(lambda (,x) ,body) (filter (lambda (a) (var-occurs-free? a expr)) (unique-vars expr))]
      [`(,rator ,rand) (union
                        (unique-free-vars rator)
                        (unique-free-vars rand))])))

(define unique-bound-vars
  (lambda (expr)
    (match expr
      [(? symbol?) '()]
      [`(lambda (,x) ,body) (filter (lambda (a) (var-occurs-bound? a expr)) (unique-vars expr))]
      [`(,rator ,rand) (union
                        (unique-bound-vars rator)
                        (unique-bound-vars rand))])))

(define listIndex
  (lambda (s ls)
    (cond
      [(null? ls) 0]
      [(eqv? s (car ls)) 0]
      [else (add1 (listIndex s (cdr ls)))])))

(define lex
  (lambda (expr scope)
    (match expr
      [(? symbol? expr) (cons 'var (cons (listIndex expr scope) '()))]
      [`(lambda (,x) ,body) `(lambda ,(lex body (cons x scope)))]
      [`(,rator ,rand) (list (lex rator scope) (lex rand scope))]
      )))

(define find-symbol-update
  (lambda (s ls)
    (cond
      [(null? ls) #f]
      [else (match (car ls)
              [`(,key . #&,value) (if (eqv? key s)
                                      (cdr (car ls))
                                      (find-symbol-update s (cdr ls)))])])
    ))

(define walk-symbol-update
  (lambda (s ls)
    (cond
      [(null? ls) s]
      [else (let ([x (find-symbol-update s ls)])
              (if x
                  (if (symbol? (unbox x))
                      (let ([y (unbox x)])
                        (set-box! x (walk-symbol-update y ls))
                        (walk-symbol-update y ls)
                        )
                      (unbox x))
                  s
                  ))])))


(define var-occurs-both?
  (lambda (s expr)
    (match expr
      [(? symbol? expr) (values (eqv? s expr) #f)]
      [`(lambda (,x) ,body) (values (var-occurs-free? s expr) (var-occurs-bound? s expr))]
      [`(,rator ,rand) (values (or (var-occurs-free? s rator) (var-occurs-free? s rand))
                               (or (var-occurs-bound? s rator) (var-occurs-bound? s rand)))]
      )))


