#lang racket

(define last-non-zero
  (lambda (ls)
    (let/cc k
      (letrec
          ((last-non-zero
            (lambda (ls)
              (cond
                [(null? ls)  '()]
                [(null? (cdr ls)) (if (eqv? (car ls) 0)
                                      (k '())
                                      ls)]
                [else (if (eqv? (car ls) 0)
                          (k (last-non-zero (cdr ls)))
                          (cons (car ls) (last-non-zero (cdr ls))))]
                ))))
	(last-non-zero ls)))))

(define value-of
  (lambda (expr env)
    (match expr
      [`(const ,expr) expr]
      [`(mult ,x1 ,x2) (* (value-of x1 env) (value-of x2 env))]
      [`(sub1 ,x) (sub1 (value-of x env))]
      [`(zero ,x) (zero? (value-of x env))]
      [`(if ,test ,conseq ,alt) (if (value-of test env)
                                    (value-of conseq env)
                                    (value-of alt env))]
      [`(letcc ,body) (let/cc k
                        (value-of body (lambda (y) (if (zero? y) k (env (sub1 y))))))]
      [`(throw ,k-exp ,v-exp) ((value-of k-exp env) (value-of v-exp env))]
      [`(let ,e ,body) (let ((a (value-of e env)))
                         (value-of body (lambda (y) (if (zero? y) a (env (sub1 y))))))]
      [`(var ,expr) (env expr)]
      [`(lambda ,body) (lambda (a) (value-of body (lambda (y) (if (zero? y) a (env (sub1 y))))))]
      [`(app ,rator ,rand) ((value-of rator env) (value-of rand env))])))



(define lex
  (lambda (expr scope)
    (match expr
      ;[`,y #:when (symbol? y) (cons 'var (cons (listIndex expr scope) '()))]
      [`,y #:when (number? y) (cons 'const (cons y '()))]
      [(? symbol? expr) (cons 'var (cons (listIndex expr scope) '()))]
      [`(zero? ,nexp) `(zero ,(lex nexp scope))]
      [`(sub1 ,n) `(sub1 ,(lex n scope))]
      [`(if ,test ,then ,alt) `(if ,(lex test scope) ,(lex then scope) ,(lex alt scope))]
      [`(* ,nexp1 ,nexp2) `(mult ,(lex nexp1 scope) ,(lex nexp2 scope))]
      [`(let/cc ,x ,body) (displayln x) (displayln body) `(letcc ,(lex body (cons x scope)))]
      [`(throw ,exp1 ,exp2) `(throw ,(lex exp1 scope) ,(lex exp2 scope))]
      [`(let ([,x ,e]) ,body) `(let ,(lex e scope) ,(lex body (cons x scope)))]
      [`(lambda (,x) ,body) `(lambda ,(lex body (cons x scope)))]
      [`(,rator ,rand) `(app ,(lex rator scope) ,(lex rand scope))]
      )))

(define listIndex
  (lambda (s ls)
    (cond
      [(null? ls) 0]
      [(eqv? s (car ls)) 0]
      [else (add1 (listIndex s (cdr ls)))])))

(define value-of-cps
  (lambda (expr env k)
    (match expr
      [`(const ,expr) (apply-k k expr)]
      [`(mult ,x1 ,x2) (value-of-cps x2 env (make-outer-k-mult x1 env k))]
      [`(sub1 ,x) (value-of-cps x env (make-k-sub1 k))]
      [`(zero ,x) (value-of-cps x env (make-k-zero k))]
      [`(if ,test ,conseq ,alt) (value-of-cps test env (make-k-if conseq alt env k))]
      [`(letcc ,body) (extend-env k env (make-letcc-k body k))]
      [`(throw ,k-exp ,v-exp) (value-of-cps v-exp env (make-outer-k-throw k-exp env k)) ]
      [`(let ,e ,body) (value-of-cps e env (make-outer-k-let env body k))]
      [`(var ,expr) (apply-env env expr k)]
      [`(lambda ,body) (make-closure body env k)]
      [`(app ,rator ,rand) (value-of-cps rand env (make-outer-k-application rator env k))])))


(define make-letcc-k
  (lambda (body k)
    `(letcc-k ,body ,k)))

(define make-outer-k-mult
  (lambda (x1 env k)
    `(outer-k-mult ,x1 ,env ,k)))

(define make-inner-k-mult
  (lambda (x2 k)
    `(inner-k-mult ,x2 ,k)))

(define make-k-sub1
  (lambda (k)
    `(k-sub1 ,k)))

(define make-k-zero
  (lambda (k)
    `(k-zero ,k)))

(define make-k-if
  (lambda (conseq alt env k)
    `(k-if ,conseq ,alt ,env ,k)))

(define make-outer-k-throw
  (lambda (k-exp env k)
    `(outer-k-throw ,k-exp ,env ,k)))


(define make-inner-k-throw
  (lambda (v-exp k)
    `(inner-k-throw ,v-exp ,k)))

(define make-outer-k-let
  (lambda (env body k)
    `(outer-k-let ,env ,body ,k)))


(define make-inner-k-let
  (lambda (body k)
    `(inner-k-let ,body ,k)))

(define make-inner-k-application
  (lambda (rand^ k)
    `(inner-k-application ,rand^ ,k)))

(define make-outer-k-application
  (lambda (rator env k)
    `(outer-k-application ,rator ,env ,k)))

(define make-k-extendedenv
  (lambda (body k)
    `(k-extendenv ,body ,k)))


(define make-closure
  (lambda (body env k)
    (apply-k k `(clos ,body ,env))))

(define extend-env
  (lambda (a^ env^ k^)
    (apply-k k^ `((,a^) . ,env^))))

(define empty-env
  (lambda ()
    '()))

(define empty-k
  (lambda ()
    `(init-k)))

(define apply-closure
  (lambda (clos a k)
    (match clos
      [`(clos ,body ,env) (extend-env a env (make-k-extendedenv body k))])))

(define apply-k
  (lambda (k arg)
    (match k
      [`(init-k) arg]
      [`(inner-k-application ,rand^ ,k) (apply-closure arg rand^ k)]
      [`(outer-k-application ,rator ,env ,k) (value-of-cps rator env (make-inner-k-application arg k))]
      [`(inner-k-let ,body ,k) (value-of-cps body arg k)]
      [`(outer-k-let ,env ,body ,k) (extend-env arg env (make-inner-k-let body k))]
      [`(inner-k-throw ,v-exp ,k) (apply-k arg v-exp)]
      [`(outer-k-throw ,k-exp ,env ,k) (value-of-cps k-exp env (make-inner-k-throw arg k))]
      [`(k-if ,conseq ,alt ,env ,k) (if arg
                                        (value-of-cps conseq env k)
                                        (value-of-cps alt env k))]
      [`(k-zero ,k) (apply-k k (zero? arg))]
      [`(k-sub1 ,k) (apply-k k (sub1 arg))]
      [`(inner-k-mult ,x2 ,k) (apply-k k (* arg x2))]
      [`(outer-k-mult ,x1 ,env ,k) (value-of-cps x1 env (make-inner-k-mult arg k))]
      [`(k-extendenv ,body ,k) (value-of-cps body arg k)]
      [`(letcc-k ,body ,k) (value-of-cps body arg k)]
      )))


(define apply-env
  (lambda (env y k)
    (match env
      [`() (apply-k k '())]
      [`((,arg) . ,oldEnv) (if (zero? y)
                               (apply-k k arg)
                               (apply-env oldEnv (sub1 y) k))])))


(define-syntax cons$
  (syntax-rules ()
    ((cons$ x y) (cons x (delay y)))))

(define car$ car)

(define cdr$
  (lambda ($) (force (cdr $))))

(define take$
  (lambda (n $)
    (cond
      ((zero? n) '())
      (else (cons (car$ $) 
                  (let ((n- (sub1 n)))
                    (cond
                      ((zero? n-) '())
                      (else (take$ n- (cdr$ $))))))))))


(define trib
  (lambda (n-3 n-2 n-1)
    (let ([sum (+ n-1 (+ n-2 n-3))])
      (cons$ n-3 (cons$ n-2 (cons$ n-1 (trib sum (+ sum (+ n-2 n-1)) (+ (+ sum (+ sum (+ n-2 n-1)) n-1)))))))
    ))

(define trib$
  (trib 0 1 1))
