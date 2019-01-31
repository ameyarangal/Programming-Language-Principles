#lang racket


(define empty-env
  (lambda ()
    (lambda (y) (error 'unbound-var "undefined ~s" y))))

(define extend-env
  (lambda (x arg env)
    (lambda (y) (if (eqv? y x) arg (apply-env env y)))))

(define apply-env
  (lambda (env y)
    (env y)))

(define make-closure
  (lambda (x body env)
    (lambda (arg) (val-of-cbv body (extend-env x arg env)))))

(define apply-closure
  (lambda (clos arg)
    (clos arg)))

(define val-of-cbv
  (lambda (exp env)
    ;(displayln exp)
    (match exp
      [`,y #:when (symbol? y) (apply-env env y)]
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(quote ,v) v]
      [`(zero? ,n) (zero? (val-of-cbv n env))]
      [`(sub1 ,n) (sub1 (val-of-cbv n env))]
      [`(* ,n1 ,n2) (* (val-of-cbv n1 env) (val-of-cbv n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbv test env)
                                    (val-of-cbv conseq env)
                                    (val-of-cbv alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env))]
      [`(set! ,x ,b) (set! x (val-of-cbv b env))]
      [`(let ([,x ,e]) ,body) (val-of-cbv body (extend-env x (val-of-cbv e env) env))]
      [`(cons ,s ,ls) (cons (val-of-cbv s env) (val-of-cbv ls env))]
      [`(cdr ,ls) (cdr (val-of-cbv ls env))]
      [`(car ,ls) (car (val-of-cbv ls env))]
      [`(add1 ,n) (add1 (val-of-cbv n env))]
      [`(null? ,ls) (null? (val-of-cbv ls env))]
      [`(random ,n) (random (val-of-cbv n env))]
      [`(cons^ ,s ,ls) (cons (lambda () (val-of-cbv s env)) (lambda () (val-of-cbv ls env)))]
      [`(car^ ,ls) ((car (val-of-cbv ls env)))]
      [`(cdr^ ,ls) ((cdr (val-of-cbv ls env)))]
      [`(lambda (,x) ,body) (make-closure x body env)]
      [`(,rator ,rand) (apply-closure (val-of-cbv rator env)
                                      (val-of-cbv rand env))])))

(define make-closure-cbr
  (lambda (x body env)
    (lambda (arg) (val-of-cbr body (extend-env x arg env)))))


(define val-of-cbr
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y) (unbox (apply-env env y))]
      [`,n #:when (number? n) n]
      [`,b #:when (boolean? b) b]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbr e1 env) (val-of-cbr e2 env))]
      [`(set! ,x ,e)
       (set-box! (env x) (val-of-cbr e env))]
      [`(if ,test ,then ,alt) (if (val-of-cbr test env)
                                  (val-of-cbr then env)
                                  (val-of-cbr alt env))]
      [`(zero? ,n) (zero? (val-of-cbr n env))]
      [`(sub1 ,n) (sub1 (val-of-cbr n env))]
      [`(lambda (,x) ,body) (make-closure-cbr x body env)]
      [`(,rator ,x) #:when (symbol? x) (apply-closure (val-of-cbr rator env) (apply-env env x))]
      [`(,rator ,rand) (apply-closure (val-of-cbr rator env) (box (val-of-cbr rand env)))])))

(define make-closure-cbname
  (lambda (x body env)
    (lambda (arg) (val-of-cbname body (extend-env x arg env)))))

(define val-of-cbname
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y) ((unbox (apply-env env y)))]
      [`,n #:when (number? n) n]
      [`,b #:when (boolean? b) b]
      [`(if ,test ,then ,alt) (if (val-of-cbname test env)
                                  (val-of-cbname then env)
                                  (val-of-cbname alt env))]
      [`(zero? ,n) (zero? (val-of-cbname n env))]
      [`(random ,n) (random (val-of-cbname n env))]
      [`(lambda (,x) ,body) (make-closure-cbname x body env)]
      [`(,rator ,x) #:when (symbol? x) (apply-closure (val-of-cbname rator env) (apply-env env x))]
      [`(,rator ,rand) (apply-closure (val-of-cbname rator env) (box (lambda () (val-of-cbname rand env))))])))

(define make-closure-cbneed
  (lambda (x body env)
    (lambda (arg) (val-of-cbneed body (extend-env x arg env)))))

(define val-of-cbneed
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y) (let ([b (apply-env env y)])
                                (let ([v ((unbox b))])
                                  (set-box! b (lambda () v))
                                  v))]
      [`,n #:when (number? n) n]
      [`,b #:when (boolean? b) b]
      [`(if ,test ,then ,alt) (if (val-of-cbneed test env)
                                  (val-of-cbneed then env)
                                  (val-of-cbneed alt env))]
      [`(zero? ,n) (zero? (val-of-cbneed n env))]
      [`(random ,n) (random (val-of-cbneed n env))]
      [`(lambda (,x) ,body) (make-closure-cbneed x body env)]
      [`(,rator ,x) #:when (symbol? x) (apply-closure (val-of-cbneed rator env) (apply-env env x))]
      [`(,rator ,rand) (apply-closure (val-of-cbneed rator env) (box (lambda () (val-of-cbneed rand env))))])))


