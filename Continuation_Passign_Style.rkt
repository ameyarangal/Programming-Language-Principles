#lang racket

(require racket/trace)

(define fact
  (lambda (n)
    (cond
      ((< n 2) 1)
      (else (* n (fact (sub1 n)))))))


(define fact-cps
  (lambda (n k)
    (cond
      [(< n 2) (k 1)]
      [else (fact-cps (sub1 n) (lambda (v) (k (* n v))))])))

#;
(define fib
  (lambda (n)
    (cond
      [(eqv? n 0) 1]
      [(eqv? n 1) 1]
      [else (+ (fib (sub1 n)) (fib (- n 2)))])))


(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only
	    (error 'empty-k "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))



(define  binary-to-decimal-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 0)]
      [else (binary-to-decimal-cps
             (cdr ls)
             (lambda (v) (k (+ (car ls) (* 2 v)))))])))

(define binary-to-decimal
  (lambda (ls)
    (binary-to-decimal-cps ls (empty-k))))

(define times
  (lambda (ls)
    (times-cps ls (empty-k))))

(define times-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) (k 0)]
      [else (times-cps
             (cdr ls)
             (lambda (v) (k (* (car ls) v))))])))

(define times-cps-shortcut
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) 0]
      [else (times-cps-shortcut
             (cdr ls)
             (lambda (v) (k (* (car ls) v))))])))

(define times-shortcut
  (lambda (ls)
    (times-cps-shortcut ls (empty-k))))

(define plus-cps
  (lambda (m k)
    (k (lambda (n)
         (+ m n)))))

(define plus
  (lambda (m)
    (plus-cps m (empty-k))))

(define remv-first-9*-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k '())]
      [(pair? (car ls))  
       (remv-first-9*-cps (car ls)
                          (lambda (a) (if (equal? (car ls) a)
                                     (remv-first-9*-cps (cdr ls)
                                                        (lambda (d) (k (cons (car ls) d))))
                                     (remv-first-9*-cps (car ls)
                                                        (lambda (a) (k (cons a (cdr ls))))))))]
      [(eqv? (car ls) '9) (k (cdr ls))]
      [else (remv-first-9*-cps (cdr ls) (lambda (d) (k (cons (car ls) d))))])))

(define remv-first-9*
  (lambda (ls)
    (remv-first-9*-cps ls (empty-k))))


(define cons-cell-count-cps
  (lambda (ls k)
    (cond
      [(pair? ls)
       (cons-cell-count-cps
        (car ls)
        (lambda (a)
          (cons-cell-count-cps (cdr ls)
                               (lambda (d)
                                 (k (add1 (+ a d)))))))]
      [else (k 0)])))

(define cons-cell-count
  (lambda (ls)
    (cons-cell-count-cps ls (empty-k))))

(define find-cps
  (lambda (u s k)
    (let ([pr (assv u s)])
      (if pr
          (find-cps (cdr pr) s k)
          (k u)))))

(define find
  (lambda (u s)
    (find-cps u s (empty-k))))


(define ack-cps
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack-cps (sub1 m) 1 k)]
      [else (ack-cps
             m
             (sub1 n)
             (lambda (n^) (ack-cps
                      (sub1 m)
                      n^
                      k)))])))

(define ack
  (lambda (m n)
    (cond
      [(zero? m) (add1 n)]
      [(zero? n) (ack (sub1 m) 1)]
      [else (ack (sub1 m)
                 (ack m (sub1 n)))])))


(define fib
  (lambda (n)
    ((lambda (fib)
       (fib fib n))
     (lambda (fib n)
       (cond
	 [(zero? n) 0]
	 [(zero? (sub1 n)) 1]
	 [else (+ (fib fib (sub1 n)) (fib fib (sub1 (sub1 n))))])))))

(define fib-cps
  (lambda (n k)
    ((lambda (fib k1)
       (fib fib n k1))
     (lambda (fib n k2)
       (cond
         [(zero? n) (k2 0)]
         [(zero? (sub1 n)) (k2 1)]
         [else (fib fib (sub1 (sub1 n))
                    (lambda (n2) (fib fib (sub1 n)
                                 (lambda (n1) (k2 (+ n1 n2))))))])) k)))
(define null?-cps
  (lambda (ls k)
    (k (null? ls))))

(define car-cps
  (lambda (pr k)
    (k (car pr))))

(define cdr-cps
  (lambda (pr k)
    (k (cdr pr))))


(define unfold
  (lambda (p f g seed)
    ((lambda (h)
       ((h h) seed '()))
     (lambda (h)
       (lambda (seed ans)
	 (if (p seed)
	     ans
	     ((h h) (g seed) (cons (f seed) ans))))))))

(define unfold-cps
  (lambda (p f g seed k)
    ((lambda (h k1)
       (h h (lambda (h^) (h^ seed '() k1))))
     (lambda (h k2)
       (k2 (lambda (seed ans k3)
             (p seed (lambda (p^) (if p^
                                 (k3 ans)
                                 (h h
                                    (lambda (h^^) (g seed
                                                (lambda (g^) (f seed
                                                           (lambda (f^) (h^^ g^ (cons f^ ans) k3)))))))
                                 )))
             )))
     k)))

(define empty-s
  (lambda ()
    '()))

(define unify
  (lambda (u v s)
    (cond
      ((eqv? u v) s)
      ((number? u) (cons (cons u v) s))
      ((number? v) (unify v u s))
      ((pair? u)
       (if (pair? v)
	   (let ((s (unify (find (car u) s) (find (car v) s) s)))
             (if s (unify (find (cdr u) s) (find (cdr v) s) s) #f))
	   #f))
      (else #f))))

(define unify-cps
  (lambda (u v s k)  
    (cond
      [(eqv? u v) (k s)]
      [(number? u) (k (cons (cons u v) s))]
      [(number? v) (unify-cps v u s k)]
      [(pair? u) (if (pair? v) 
                     (find-cps (car u) s
                               (lambda (au) (find-cps (car v) s
                                                 (lambda (av) (unify-cps au av s
                                                                    (lambda (s) (if s
                                                                               (find-cps (cdr u) s
                                                                                         (lambda (du) (find-cps (cdr v) s
                                                                                                           (lambda (dv) (unify-cps du dv s k)))))
                                                                               (k #f))))))))
                     (k #f))]
      [else (k #f)])))


(unify-cps '(x y) '(5 6) (empty-s) (empty-k))

(define M
  (lambda (f)
    (lambda (ls)
      (cond
        ((null? ls) '())
        (else (cons (f (car ls)) ((M f) (cdr ls))))))))



(define M-cps
  (lambda (f k)
    (k (lambda (ls k1)      
         (cond
           [(null? ls) (k1 '())]
           [else (f (car ls)
                    (lambda (a) (M-cps f
                                  (lambda (m^) (m^ (cdr ls)
                                              (lambda (d) (k1 (cons a d)))))
                                  )))])))))


(define use-of-M-cps
  (M-cps (lambda (n k) (k (add1 n))) (lambda (f) (f '(1 2 3 4 5) (empty-k)))))

(displayln 'use-of-M-cps)

use-of-M-cps

(define strange
  (lambda (x)
    ((lambda (g) (lambda (x) (g g)))
     (lambda (g) (lambda (x) (g g))))))
#;
(trace strange)

(define strange-cps
  (lambda (x k)
    ((lambda (g k1) (k1 (lambda (x k2) (g g k2))))
     (lambda (g k3) (k3 (lambda (x k4) (g g k4))))
     k)))
#;
(trace strange-cps)

(define use-of-strange
  (let ([strange^ (((strange 5) 6) 7)])
    (((strange^ 8) 9) 10)))

(displayln 'use-of-strange)

use-of-strange

(define use-of-strange-cps
  (let ([strange^ (strange-cps 5 (lambda (f^) (f^ 6 (lambda (f^^) (f^^ 7 (empty-k))))))])
    (strange^ 8 (lambda (v) (v 9 (lambda (v^) (v^ 10 (empty-k))))))))

(displayln  'use-ofstrange-cps)

use-of-strange-cps

(define why
  (lambda (f)
    ((lambda (g)
       (f (lambda (x) ((g g) x))))
     (lambda (g)
       (f (lambda (x) ((g g) x)))))))

(define why-cps
  (lambda (f k)
    ((lambda (g k1)
       (f (lambda (x k2) (g g
                       (lambda (g^) (g^ x k2))))
          k1))
     (lambda (g k3)
       (f (lambda (x k4) (g g
                       (lambda (g^) (g^ x k4)))) k3))
     k)))

(define almost-length
  (lambda (f)
    (lambda (ls)
      (if (null? ls)
          0
          (add1 (f (cdr ls)))))))

(define almost-length-cps
  (lambda (f k)
    (k (lambda (ls k1)
         (if (null? ls)
             (k1 0)
             (f (cdr ls) (lambda (d) (k1 (add1 d)))))))))



(why-cps almost-length-cps (lambda (v) (v '(a b c d e) (empty-k))))




;(trace strange)

;use-of-strange

;; (trace find-cps)
;; (trace times-cps-shortcut)
;;(find-cps 7 '((5 . a) (6 . 5) (7 . 6)) (empty-k))

;; (times-cps-shortcut '(1 2 3 0 5) (empty-k))
;; (fact 5)
;; (fact-cps 5 (lambda (v) v))
