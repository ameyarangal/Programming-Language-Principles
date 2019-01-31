#lang racket

(define lex
  (lambda (expr scope)
    (match expr
      [(? symbol? expr) (cons 'var (cons (listIndex expr scope) '()))]
      [`,y #:when (number? y) (cons 'const (cons y '()))]
      [`(sub1 ,e) `(sub1 ,(lex e scope))]
      [`(zero? ,e) `(zero? ,(lex e scope))]
      [`(if ,test ,positive ,negative) `(if ,(lex test scope) ,(lex positive scope) ,(lex negative scope))]
      [`(* ,e1 ,e2) `(* ,(lex e1 scope) ,(lex e2 scope))]
      [`(let [(,x ,e1)] ,body) `(let ,(lex e1 scope) ,(lex body scope))]
      [`(lambda (,x) ,body) `(lambda ,(lex body (cons x scope)))]
      [`(,rator ,rand) (list (lex rator scope) (lex rand scope))]
      )))

(define listIndex
  (lambda (s ls)
    (cond
      [(null? ls) 0]
      [(eqv? s (car ls)) 0]
      [else (add1 (listIndex s (cdr ls)))])))

(define empty-env
  (lambda ()
    (lambda (y) (error 'value-of "unbound variable ~s" y))))


(define apply-env
  (lambda (env y)
    (env y)))

(define extend-env
  (lambda (x arg env)
    (lambda (y) (if (eqv? y x) arg (apply-env env y)))))

(define apply-closure-fn
  (lambda (clos arg)
    (clos arg)))

(define build-closure-fn
  (lambda (body x env)
    (lambda (arg) (value-of-fn body (extend-env x arg env)) )))


(define value-of-fn
  (lambda (e env)
    (match e
      [`,y #:when (symbol? y) (apply-env env y)]
      [`,n #:when (number? n) n]
      [`,y #:when (boolean? y) y] ;boolean
      [`(zero? ,expr) (if (zero? (value-of-fn expr env)) #t #f)] ;zero
      [`(if ,test ,then ,alt) (if (value-of-fn test env)
                                  (value-of-fn then env)
                                  (value-of-fn alt env))] ;if statement
      [`(sub1 ,expr) (sub1 (value-of-fn expr env))] ; sub1
      [`(* ,expr1 ,expr2) (* (value-of-fn expr1 env) (value-of-fn expr2 env))]
      [`(let ([,x ,expr1]) ,body) (value-of-fn body (extend-env x (value-of-fn expr1 env) env))]
      [`(lambda (,x) ,body) #:when (symbol? x) (build-closure-fn body x env)]
      [`(,rator ,rand) (apply-closure-fn (value-of-fn rator env) (value-of-fn rand env))])))


(define build-closure-ds
  (lambda (body x env)
    `(clos ,body ,x ,env)))

(define apply-closure-ds
  (lambda (clos arg)
    (match clos
      [`(clos ,body ,x ,env) (value-of-fn body (extend-env x arg env))])))

(define value-of-ds
  (lambda (e env)
    (match e
      [`,y #:when (symbol? y) (apply-env env y)]
      [`,n #:when (number? n) n]
      [`,y #:when (boolean? y) y] ;boolean
      [`(zero? ,expr) (if (zero? (value-of-ds expr env)) #t #f)] ;zero
      [`(if ,test ,then ,alt) (if (value-of-ds test env)
                                  (value-of-ds then env)
                                  (value-of-ds alt env))] ;if statement
      [`(sub1 ,expr) (sub1 (value-of-ds expr env))] ; sub1
      [`(* ,expr1 ,expr2) (* (value-of-ds expr1 env) (value-of-ds expr2 env))]
      [`(let ([,x ,expr1]) ,body) (value-of-ds body (extend-env x (value-of-ds expr1 env) env))]
      [`(lambda (,x) ,body) #:when (symbol? x) (build-closure-ds body x env)]
      [`(,rator ,rand) (apply-closure-ds (value-of-ds rator env) (value-of-ds rand env))])))


(define empty-env-ds
  (lambda ()
    '()))

(define apply-env-ds
  (lambda (env y)
    (match env
      [`() '()]
      [`((,x . ,arg) . ,oldEnv) (if (eqv? y x) arg (apply-env-ds oldEnv y))])))

(define extend-env-ds
  (lambda (x arg env)
    `((,x . ,arg) . ,env)))


(define value-of-dynamic
  (lambda (e env)
    (match e
      [`,y #:when (symbol? y)
           ;(displayln y)
           ;(displayln (apply-env env y))
           (apply-env-ds env y)]
      [`(quote ,y) y]
      [`,n #:when (number? n) n]
      [`(if ,test ,then ,alt)
       (if (value-of-dynamic test env)
           (value-of-dynamic then env)
           (value-of-dynamic alt env))]
      [`(* ,n1 ,n2) 
       (* (value-of-dynamic n1 env) (value-of-dynamic n2 env))]
      [`(sub1 ,n1) (sub1 (value-of-dynamic n1 env))]
      [`(null? ,e1) (null? (value-of-dynamic e1 env))]
      [`(zero? ,n) (zero? (value-of-dynamic n env))]
      [`(cons ,s ,ls) (cons (value-of-dynamic s env) (value-of-dynamic ls env))]
      [`(cdr ,ls) (cdr (value-of-dynamic ls env))]
      [`(car ,ls) (car (value-of-dynamic ls env))]
      [`(let ([,x ,expr1]) ,body)
       (value-of-dynamic body (extend-env-ds x (value-of-dynamic expr1 env) env))]
      [`(lambda (,x) ,body)
       `(lambda (,x) ,body)]
      [`(,rator ,rand)
       (let ([f (value-of-dynamic rator env)]
             [arg (value-of-dynamic rand env)])
         (match f
           [`(lambda (,x) ,body) (value-of-dynamic body (extend-env-ds x arg env))]))]
      )))

(define empty-env-fn
  (lambda ()
    (lambda (y) (error 'value-of "unbound identifier ~s" y))))

(define extend-env-fn
  (lambda (x arg env)
    (lambda (y) (if (eqv? y x) arg (apply-env-fn env y)))))

(define apply-env-fn
  (lambda (env y)
    (env y)))

(define closure-fn-ri
  (lambda (body x env func)
    (lambda (arg) (func arg))))

(define apply-closure-fn-ri
  (lambda (clos arg func)
    (clos arg)))

(define closure-ds-ri
  (lambda (body x env func)
    `(clos ,body ,x ,env)))

(define apply-closure-ds-ri
  (lambda (clos arg func)
    (match clos
      [`(clos ,body ,x ,env) (func arg body x env)])))

(define value-of-ri
  (lambda (env extendEnv applyEnv clos closApply)
    (lambda (expr)
      (letrec ([f (lambda (e env) (match e
                               [`,y #:when (symbol? y) (applyEnv env y)]
                               [`,n #:when (number? n) n]
                               [`(if ,test ,positive ,negative)
                                (if (f test env)
                                    (f positive env)
                                    (f negative env))]
                               [`(* ,n1 ,n2) (* (f n1 env) (f n2 env))]
                               [`(sub1 ,n1) (sub1 (f n1 env))]
                               [`(zero? ,n1) (zero? (f n1 env))]
                               [`,y #:when (boolean? y) y]
                               [`(let ([,x ,e1]) ,body) (let ([y (f e1 env)]) (f body env))]
                               [`(lambda (,x) ,body)
                                (clos body x env (lambda (arg) (f body (extendEnv x arg env))))]
                               [`(,rator ,rand)
                                (closApply
                                 (f rator env)
                                 (f rand env)
                                 (lambda (arg b x en) (f b (extendEnv x arg en))))]))])
        (f expr env)))))
