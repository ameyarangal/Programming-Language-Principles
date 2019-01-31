#lang scheme
(define plus
  (lambda (x y)
    (cond
     [(zero? y) x]
     [else (add1 (plus x (sub1 y)))]
     )))

(define countdown
  (lambda (n)
    (cond
      [(zero? n) '(0)]
      [else (cons n (countdown(sub1 n)))])))

(define insertR
  (lambda (s1 s2 ls)
    (cond
      [(null? ls) '()]
      [else
       (cond
         [(eqv? s1 (car ls)) (cons s1 (cons s2 (insertR s1 s2 (cdr ls))))]
         [else (cons (car ls) (insertR s1 s2 (cdr ls)))]
         )]
      )))

(define remv-1st
  (lambda (s1 ls)
    (cond
      [(null? ls) '()]
      [else
       (cond
         [(eqv? s1 (car ls)) (cdr ls)]
         [else (cons (car ls) (remv-1st s1 (cdr ls)))] 
         )]
      )))

(define list-index-ofv?
  (lambda (s1 ls)
    (cond
      [(null? ls) 0]
      [else
       (cond
         [(eqv? s1 (car ls)) 0]
         [else (add1 (list-index-ofv? s1 (cdr ls)))]
         )]
      )))

(define filter
  (lambda (p ls)
    (cond
      [(null? ls) '()]
      [else
       (cond
         [(p (car ls)) (cons (car ls) (filter p (cdr ls)))]
         [else (filter p (cdr ls))]
         )]
      )))

(define zip
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) '()]
      [(null? ls2) '()]
      [else (cons (cons (car ls1) (car ls2)) (zip (cdr ls1) (cdr ls2)))]
      )))

(define map
  (lambda (p ls)
    (cond
      [(null? ls) '()]
      [else (cons (p ( car ls)) (map p (cdr ls)))]
      )))

(define append
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) ls2]
      [else (cons (car ls1) (append (cdr ls1) ls2))]
      )))

(define lastElement
  (lambda (ls)
    (cond
      [(null? ls) null]
      [(null? (cdr ls)) (car ls)]
      [else (lastElement (cdr ls))]
      )))

(define reverse
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(null? (cdr ls)) ls]
      [else (append (reverse (cdr ls)) (cons (car ls) '()))]
      )))

(define multiply
  (lambda (n1 n2)
    (cond
      [(zero? n2) 0]
      [else (+ n1 (multiply n1 (sub1 n2)))]
      )))

(define fact
  (lambda (n1)
    (cond
      [(zero? n1) 1]
      [else (multiply n1 (fact (sub1 n1)))]
      )))

(define memv
  (lambda (s1 ls)
    (cond
      [(null? ls) #f]
      [else
       (cond
         [(eqv? s1 (car ls)) ls]
         [else (memv s1 (cdr ls))]
         )]
      )))

(define fib
  (lambda (n1)
    (cond
      [(zero? n1) 0]
      [(eqv? n1 1) 1]
      [else (+ (fib (sub1 n1)) (fib (sub1 (sub1 n1))))]
      )))

(define power
  (lambda (n1 n2)
    (cond
      [(zero? n2) 1]
      [else (multiply n1 (power n1 (sub1 n2)))]
      )))


(define binary->natural
  (lambda (ls)
    (cond
      [(null? ls) 0]
      [else (+
             (cond
               [(zero? (car ls)) 0]
               [(eqv? (car ls) 1) 1])
             (multiply 2 (binary->natural (cdr ls))))]
      )))
               
 (define minus
   (lambda (n1 n2)
     (cond
       [(zero? n2) n1]
       [else (sub1 (minus n1 (sub1 n2)))]
       )))

(define div
  (lambda (n1 n2)
    (cond
      [(zero? n1) 0]
      [else (add1 (div (minus n1 n2) n2))]
      )))

(define append-map
  (lambda (p ls)
    (cond
      [(null? ls) '()]
      [else (append (p (car ls)) (append-map p (cdr ls)))]
      )))

(define find?
  (lambda (s1 ls)
    (cond
      [(null? ls) #f]
      [else
       (cond
         [(eqv? s1 (car ls)) #t]
         [else (find? s1 (cdr ls))]
         )])))


(define set-difference
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) '()]
      [else
       (cond
         [(not(find? (car ls1) ls2)) (cons (car ls1) (set-difference (cdr ls1) ls2))]
         [else (set-difference (cdr ls1) ls2)]
         )])))

(define powerset
  (lambda (ls)
    (cond
      [(null? ls) '(())]
      [else
       (append
        (map (lambda (x) (cons (car ls) x)) (powerset (cdr ls)))
        (powerset (cdr ls)))]
      )))

(define cartesian-product
  (lambda (ls)
    (cond
      [(null? (car ls)) '()]
      [else
       (append
        (map (lambda (x) (cons (car (car ls)) (cons x '()))) (car (cdr ls)))
        (cartesian-product (cons (cdr (car ls)) (cdr ls)))
        )])))

(define insertR-fr
  (lambda (s1 s2 ls)
    (cond
      [(null? ls) '()]
      [else (foldr
             (lambda (x result)
               (cond
                 [(eqv? s1 x) (cons x (cons s2 result))]
                 [else (cons x result)]))
             '()
             ls)])))

(define filter-fr
  (lambda (p ls)
    (cond
      [(null? ls) '()]
      [else (foldr
             (lambda (x result)
               (cond
                 [(p x) (cons x result)]
                 [else result]
                 ))
             '()
             ls)])))

(define map-fr
 (lambda (p ls)
   (cond
     [(null? ls) '()]
     [else (foldr
            (lambda (x result) (cons (p x) result))
            '()
            ls)])))

(define append-fr
 (lambda (ls1 ls2)
   (cond
     [(null? ls1) ls2]
     [(null? ls2) ls1]
     [else (foldr cons ls2 ls1)])))

;(lambda (x result) (cons x result))

(define reverse-fr
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [else (foldr
             (lambda (x result) (append result (cons x '())))
             '()
             ls)])))

(define binary->natural-fr
  (lambda (ls)
    (cond
      [(null? ls) 0]
      [else (foldr
             (lambda (x result) (+ x (multiply 2 result)))
             0
             ls)])))

(define append-map-fr
  (lambda (p ls)
    (cond
      [(null? ls) '()]
      [else (foldr
             (lambda (x result) (append (p x) result))
             '()
             ls)])))

(define set-difference-fr
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) '()]
      [(null? ls2) ls1]
      [else (foldr
             (lambda (x result)
               (cond
                 [(find? x ls2) result]
                 [else (cons x result)]))
             '()
             ls1)])))             

(define powerset-fr
  (lambda (ls)
    (cond
      [(null? ls) '(())]
      [else (foldr
             (lambda (x result)
               (append
                result
                (foldr
                 (lambda (y intermediate_result)
                   (cons 
                    (cons x y)
                    intermediate_result))
                 '()
                 result)))
             '(())
             ls)])))    
             
(define cartesian-product-fr
  (lambda (ls)
    (cond
      [(null? (car ls)) '()]      
      [else (foldr
             (lambda (x1 result)
               (append
                result
                (foldr
                 (lambda (x2 intermediate_result)
                   (cons
                    (cons x1 (cons x2 '()))
                    intermediate_result
                    ))
                 '()
                 (car (cdr ls))
                 )))
             '()
             (car ls)
             )])))

(define collatz
  (lambda (n)
    (letrec
        ((odd-case
          (lambda (recur)
            (lambda (x)
              (cond 
                ((and (positive? x) (odd? x)) (collatz (add1 (* x 3)))) 
                (else (recur x))))))
         (even-case
          (lambda (recur)
            (lambda (x)
              (cond 
                ((and (positive? x) (even? x)) (collatz (/ x 2))) 
                (else (recur x))))))
         (one-case
          (lambda (recur)
            (lambda (x)
              (cond
                ((zero? (sub1 x)) 1)
                (else (recur x))))))
         (base
          (lambda (x)
            (error 'error "Invalid value ~s~n" ))))
      (if (positive? n) ((if (zero? (sub1 n)) (one-case base) (if (even? n) (even-case odd-case) (odd-case even-case))) n) (base n))
      ) 
    )
  )

(define quine
  (lambda (x y)
    (list (list 'quote x) (list 'quote y))))
