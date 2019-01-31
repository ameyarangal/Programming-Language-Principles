#lang racket

(require "monads.rkt")

(define findf-maybe
  (lambda (p ls)
    (cond
      [(null? ls) (Nothing)]
      [else (if (p (car ls))
                (Just (car ls))
                (findf-maybe p (cdr ls)))])))


(define partition-writer
  (lambda (p ls)
    (displayln ls)
    (cond
      [(null? ls) (inj-writer '())]
      [else (if (p (car ls))  
                (bind-writer
                 (partition-writer p (cdr ls))
                 (lambda (d) (inj-writer `(,(car ls) . ,d))))
                (bind-writer
                 (tell (car ls))
                 (lambda (_) (partition-writer p (cdr ls)))))])))

(define power
  (lambda (x n)
    (cond
      [(zero? n) 1]
      [(zero? (sub1 n)) x]
      [(odd? n) (* x (power x (sub1 n)))]
      [(even? n) (let ((nhalf (/ n 2)))
                   (let ((y (power x nhalf)))
                     (* y y)))])))

(define powerXpartials
  (lambda (x n)
    (cond
      [(zero? n) (inj-writer 1)]
      [(zero? (sub1 n)) (inj-writer x)]
      [(odd? n) (bind-writer
                 (powerXpartials x (sub1 n))
                 (lambda (r)
                   (bind-writer
                    (tell r)
                    (lambda (_) (inj-writer (* x r))))))]
      [(even? n) (let ((nhalf (/ n 2)))
                   (bind-writer
                    (powerXpartials x nhalf)
                    (lambda (y)
                      (bind-writer
                       (tell y)
                       (lambda (_) (inj-writer (* y y))))))
                   
                   )])))

(define replace-with-count
  (lambda (x tr)
    (displayln tr)
    (cond
      [(null? tr) (inj-state '())]
      [(symbol? tr) (if (equal? x tr)
                        (go-on ([v (get)]
                                [_ (put (+ v 1))])
                               (inj-state v))
                        (inj-state tr))]
      
      
      [else (go-on ([a (replace-with-count x (car tr))]
                    [d (replace-with-count x (cdr tr))]
                    )
                   (inj-state (cons a d)))]
      
      )))

(define traverse
  (lambda (inj bind f)
    (letrec
        ((trav
          (lambda (tree)
            (cond
              [(pair? tree)
               (go-on ([a (trav (car tree))]
                       [d (trav (cdr tree))])
                      (inj (cons a d)))]
              [else (f tree)]))))
      trav)))

(define reciprocal
  (lambda (n)
    (cond
      [(zero? n) (Nothing)]
      [else (Just (/ 1 n))])))

(define traverse-reciprocal
  (traverse Just bind-maybe reciprocal))


(define halve
  (lambda (n)
    (cond
      [(zero? (remainder n 2)) (inj-writer (/ n 2))]
      [else (bind-writer
             (tell n)
             (lambda (_) (inj-writer n)))])))

(define traverse-halve
  (traverse inj-writer bind-writer halve))


(define state/sum
  (lambda (n)
    (go-on ([v (get)]
            [_ (put (+ v n))])
           (inj-state v))))

(define traverse-state/sum
  (traverse inj-state bind-state state/sum))



(define value-of-cps
  (lambda (expr env)
    (match expr
      [(? number?) (inj-cont expr)]
      [(? boolean?) (inj-cont expr)]      
      [(? symbol?) (inj-cont (apply-env env expr))]
      [`(* ,x1 ,x2) (go-on ([x1^ (value-of-cps x1 env)]
                            [x2^ (value-of-cps x2 env)])
                           (inj-cont (* x1^ x2^ )))]
      [`(sub1 ,x) (go-on ([x^ (value-of-cps x env)])
                         (inj-cont (sub1 x^)))]
      [`(zero? ,x) (go-on ([x^ (value-of-cps x env)])
                          (inj-cont (zero? x^)))]
      [`(if ,test ,conseq ,alt) (go-on ([test^ (value-of-cps test env)])
                                       (if test^
                                           (value-of-cps conseq env)
                                           (value-of-cps alt env)))]
      [`(capture ,k-id ,body)(callcc (lambda (k)
                                       (value-of-cps body (extend-env k-id k env))))]
      [`(return ,k-exp ,v-exp) (go-on ([kexp (value-of-cps k-exp env)]
                                       [vexp (value-of-cps v-exp env)])
                                      (kexp vexp))]
      [`(lambda (,id) ,body) (inj-cont (closure id body env))]
      [`(,rator ,rand) (go-on ([rand^ (value-of-cps rand env)]
                               [rator^ (value-of-cps rator env)])
                              (apply-proc rator^ rand^))])))


(define empty-env
  (lambda ()
    (lambda (y) (error 'value-of "unbound variable ~s" y))))


(define apply-env
  (lambda (env y)
    (env y)))

(define extend-env
  (lambda (x arg env)
    (lambda (y) (if (eqv? y x) arg (apply-env env y)))))

(define apply-proc
  (lambda (clos arg)
    (clos arg)))

(define closure
  (lambda (x body env)
    (lambda (arg) (value-of-cps body (extend-env x arg env)))))


(define fact-5
  '((lambda (f)
      ((f f) 5))
    (lambda (f)
      (lambda (n)
        (if (zero? n)
            1
            (* n ((f f) (sub1 n))))))))


(define capture-fun
    '(* 3 (capture q (* 2 (return q 4)))))


