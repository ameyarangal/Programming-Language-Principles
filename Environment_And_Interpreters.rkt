#lang racket



(define value-of
  (lambda (e env)
    ;;(displayln e)
    (match e
      [`,y #:when (symbol? y) (unbox (env y))] ;symbol
      [`,n #:when (number? n)  (displayln 'called ) n] ;number
      [`,y #:when (boolean? y) y] ;boolean
      [`(zero? ,expr) (if (zero? (value-of expr env)) #t #f)] ;zero
      [`(if ,test ,then ,alt) (if (value-of test env)
                                  (value-of then env)
                                  (value-of alt env))] ;if statement
      [`(sub1 ,expr) (sub1 (value-of expr env))] ; sub1
      [`(* ,expr1 ,expr2) (* (value-of expr1 env) (value-of expr2 env))]
      [`(begin2 ,expr1 ,expr2) (begin (value-of expr1 env)  (value-of expr2 env))]
      [`(set! ,id ,expr) (set-box! (env id) (value-of expr env))]
      [`(let ([,x ,expr1]) ,body)
       (define exprValue (box (value-of expr1 env)))
       (value-of body (lambda (y) (if (eqv? y x) exprValue (env y))) )] ;let expression
      [`(lambda (,x) ,body) #:when (symbol? x) (lambda (arg)
                                            (define b-arg (box arg))
                                            (value-of body (lambda (y) (if (eqv? y x) b-arg (env y)))))] ; lambda expr
      [`(,rator ,rand) ((value-of rator env) (value-of rand env))])))



(define empty-env-fn
  (lambda ()
    (lambda (y) (error 'value-of "unbound variable ~s" y))))


(define apply-env-fn
  (lambda (env y)
    (env y)))

(define extend-env-fn
  (lambda (x arg env)
    (lambda (y) (if (eqv? y x) arg (apply-env-fn env y)))))


(define value-of-fn
  (lambda (e env)
    (match e
      [`,y #:when (symbol? y) (apply-env-fn env y)]
      [`,n #:when (number? n) n]
      [`,y #:when (boolean? y) y] ;boolean
      [`(zero? ,expr) (if (zero? (value-of-fn expr env)) #t #f)] ;zero
      [`(if ,test ,then ,alt) (if (value-of-fn test env)
                                  (value-of-fn then env)
                                  (value-of-fn alt env))] ;if statement
      [`(sub1 ,expr) (sub1 (value-of-fn expr env))] ; sub1
      [`(* ,expr1 ,expr2) (* (value-of-fn expr1 env) (value-of-fn expr2 env))]
      [`(let ([,x ,expr1]) ,body) (value-of-fn body (extend-env-fn x (value-of-fn expr1 env) env))]
      [`(lambda (,x) ,body) #:when (symbol? x) (lambda (arg) (value-of-fn body (extend-env-fn x arg env)))]
      [`(,rator ,rand) ((value-of-fn rator env) (value-of-fn rand env))])))


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

(define value-of-ds
  (lambda (e env)
    (match e
      [`,y #:when (symbol? y) (apply-env-ds env y)]
      [`,n #:when (number? n) n]
      [`,y #:when (boolean? y) y] ;boolean
      [`(zero? ,expr) (if (zero? (value-of-ds expr env)) #t #f)] ;zero
      [`(if ,test ,then ,alt) (if (value-of-ds test env)
                                  (value-of-ds then env)
                                  (value-of-ds alt env))] ;if statement
      [`(sub1 ,expr) (sub1 (value-of-ds expr env))] ; sub1
      [`(let ([,x ,expr1]) ,body) (value-of-ds body (extend-env-ds x (value-of-ds expr1 env) env))]
      [`(* ,expr1 ,expr2) (* (value-of-ds expr1 env) (value-of-ds expr2 env))]
      [`(lambda (,x) ,body) #:when (symbol? x) (lambda (arg) (value-of-ds body (extend-env-ds x arg env)))]
      [`(,rator ,rand) ((value-of-ds rator env) (value-of-ds rand env))])))


(define apply-env
  (lambda (env y)
    (env y)))

(define extend-env
  (lambda (x arg env)
    (lambda (y) (if (eqv? y x) arg (apply-env env y)))))


(define fo-eulav
  (lambda (e env)
    (match e
      [`,s #:when (symbol? s) (apply-env env s)]
      [`,n #:when (number? n) n]
      [`,b #:when (boolean? b) b]
      [`(,expr 1bus) (sub1 (fo-eulav expr env))]
      [`(,expr2 ,expr1 *) (* (fo-eulav expr1 env) (fo-eulav expr2 env))]
      [`(,expr ?orez) (zero? (fo-eulav expr env))]
      [`(,alt ,then ,test fi) (if (fo-eulav test env) (fo-eulav then env) (fo-eulav alt env))]
      [`(,body (,x) adbmal) (lambda (arg) (fo-eulav body (extend-env x arg env)))]
      [`(,rand ,rator) ((fo-eulav rator env) (fo-eulav rand env))])))



(define value-of-lex
  (lambda (exp env)
    (match exp
      [`(const ,expr) expr]
      [`(mult ,x1 ,x2) (* (value-of-lex x1 env) (value-of-lex x2 env))]
      [`(zero ,x) (zero? (value-of-lex x env))]
      (`(sub1 ,body) (sub1 (value-of-lex body env)))
      (`(if ,t ,c ,a) (if (value-of-lex t env) (value-of-lex c env) (value-of-lex a env)))
      (`(var ,num) (apply-env-lex env num))
      (`(lambda ,body) (lambda (a) (value-of-lex body (extend-env-lex a env))))
      (`(,rator ,rand) ((value-of-lex rator env) (value-of-lex rand env))))))

(define empty-env-lex 
  (lambda () '()))

(define extend-env-lex
  (lambda (newvar env)
    (cons newvar env)))

(define apply-env-lex
  (lambda (env num)
    (list-ref env num)))

