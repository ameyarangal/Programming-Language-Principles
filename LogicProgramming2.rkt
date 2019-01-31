#lang racket


(require "mk.rkt")
(require "numbers.rkt")


(defrel (listo ls)
  (conde
   [(== ls '())]
   ((fresh (a d)
           (== `(,a . ,d) ls)
           (listo d)))
   [(== 1 2)]))

#;
(define fact
  (lambda (n)
    (cond
      [(zero? n) 1]
      [else (* n (fact (sub1 n)))])))


#;
(define fact-binary-reverse
  (lambda (ls)
    (cond
      [(null? ls) 0]
      [(null? (cdr ls)) (if (zero? (car ls)) 0 1)]
      [else (+ (car ls) (fact-binary-reverse (cdr ls)))]))
  )

#;
(defrel (minuotest m n o)
  (minuso m n o))

#;
(defrel (facto n result)
  (conde
   [(== n '()) (== result '(1))]
   [(== n '(1)) (== result '(1))]
   [(fresh (d q r)
           (minuso n '(1) d)
           (/o result d q r)
           (facto d q))]))


(defrel (facto n result)
  (conde
   [(== n '()) (== result (build-num 1))]
   [(== n '(1)) (== result (build-num 1))]
   [(fresh (p n-)
           (minuso n '(1) n-)
           (facto n- p)
           (*o n p result))]))


(define fibs
  (lambda (n)
    (cond
      ((eqv? n 0) (values 1 1))
      (else
       (let ((n- (- n 1)))
         (let-values (((u v) (fibs n-)))
           (let ((u+v (+ u v)))
             (values v u+v))))))))

(defrel (fibso n o1 o2)
  (conde
   [(== n (build-num 0)) (== o1 (build-num 1)) (== o2 (build-num 1))]
   [(fresh (n- u v u+v)
           (minuso n '(1) n-)
           (fibso n- u v)
           (pluso u v u+v)
           (== o1 v)
           (== o2 u+v) 
           )]))

(defrel (lookupᵒ vars vals y o)
  (fresh (var val vars^ vals^)
    (== `(,var . ,vars^) vars)
    (== `(,val . ,vals^) vals)
    (condᵉ
     [(== var y) (== val o)]
     [(=/= var y) (lookupᵒ vars^ vals^ y o)])))

(define (valof exp vars vals)
  (match exp
    [`,y
     #:when (symbol? y)
     (lookupᵒ vars vals y)]
    [`(quote ,x) x]
    [`(λ (,x) ,body)
     `(clos ,x ,body ,vars ,vals)]
    [`(,rator ,rand)
     (match (valof rator vars vals)
       [`(clos ,x ,body ,vars^ ,vals^)
        (match (valof rand vars vals)
          [`,arg
           (valof body `(,x . ,vars^) `(,arg . ,vals^))])])]))


#;
(define fo-eulav
  (lambda (e env)
    (match e
      [`,s #:when (symbol? s) (apply-env env s)]
      [`,n #:when (number? n) n]
      [`,b #:when (boolean? b) b]
      [`(etouq ,v) v]
      [`(tsil ,e) (list (fo-eulav e env))]
      [`(,expr 1bus) (sub1 (fo-eulav expr env))]
      [`(,expr2 ,expr1 *) (* (fo-eulav expr1 env) (fo-eulav expr2 env))]
      [`(,expr ?orez) (zero? (fo-eulav expr env))]
      [`(,alt ,then ,test fi) (if (fo-eulav test env) (fo-eulav then env) (fo-eulav alt env))]
      [`(,body (,x) adbmal) (lambda (arg) (fo-eulav body (extend-env x arg env)))]
      [`(,rand ,rator) ((fo-eulav rator env) (fo-eulav rand env))])))

(defrel (valof*ᵒ exps vars vals val)
  (conde
   [(== '() exps) (== val '())]
   [(fresh (exp exps^)
      (== `(,exp . ,exps^) exps)
      (fresh (v vs)
        (== val `(,v . ,vs))
        (fo-lavo exp vars vals v)
        (valof*ᵒ exps^ vars vals vs)))]))

(defrel (fo-lavo exp vars vals val)
  (conde
   [(symbolo exp) (lookupᵒ vars vals exp val)]
   [(== exp `(etouq ,val))
    (absento 'closure val)
    #;
    (absento 'quote vars)]
   [(fresh (exps)
           (== exp `(tsil . ,exps))
           (absento 'list vars)
           (valof*ᵒ exps vars vals val))]
   [(fresh (x b)
           #;
           (absento 'λ vars)
           (=/= 'quote x)
           (=/= 'λ x)
           (symbolo x)
           (== `(,b (,x) adbmal) exp)
           (== val `(closure ,x ,b ,vars ,vals)))]
   [(fresh (rator rand)
           (== exp `(,rator ,rand))
           (fresh (x b vars^ vals^ a c rator-o)
                  (== `(closure ,x ,b ,vars^ ,vals^) rator-o)
                  (fo-lavo rator vars vals rator-o)
                  (fo-lavo rand vars vals a)
                  (fo-lavo b `(,x . ,vars^) `(,a . ,vals^) val)))]))


