#lang racket

(define empty-k
  (lambda ()
    `(init-k)))

(define m^ #f)
(define n^ #f)
(define k^ #f)
(define v^ #f)

(define ack
  (lambda ()  ;m n k
    (cond
      [(zero? m^) (begin
                    (set! v^ (add1 n^))
                    (apply-k-ack))]
      [(zero? n^) (begin
                    (set! n^ 1)
                    (set! m^ (sub1 m^))
                    (ack))]
      [else (begin
              (set! k^ (make-k-ack m^ k^))
              (set! n^ (sub1 n^))
              (ack))
            ])))

(define make-k-ack
  (lambda (m k)
    `(k-ack ,m ,k)
    ))

(define apply-k-ack
  (lambda () ; k v
    (match k^
      [`(init-k) v^]
      [`(k-ack ,m ,k) (begin
                        (set! k^ k)
                        (set! n^ v^)
                        (set! m^ (sub1 m))
                        (ack))]
      [else (k^ v^)])))

(define ack-reg-driver
  (lambda (m n)
    (begin
      (set! m^ m)
      (set! n^ n)
      (set! k^ (empty-k))
      (ack))))

(define empty-k-fn
  (lambda ()
    (lambda (v) v)))


(define ls-depth^ #f)
(define k-depth^ #f)
(define v-depth^ #f)


(define depth
  (lambda () ; ls k
    (cond
      [(null? ls-depth^) (begin
                           (set! v-depth^ 1)
                           (apply-k-depth))]
      [(pair? (car ls-depth^)) (begin
                                 (set! k-depth^ (make-outer-k-depth ls-depth^ k-depth^))
                                 (set! ls-depth^ (car ls-depth^))
                                 (depth))]
      [else
       (begin
         (set! ls-depth^ (cdr ls-depth^))
         (depth))])))

(define make-inner-k-depth
  (lambda (l k)
    `(inner-k-depth ,l ,k)))

(define make-outer-k-depth
  (lambda (ls k)
    `(outer-k-depth ,ls ,k)))

(define apply-k-depth
  (lambda () ;k v
    (match k-depth^
      [`(init-k) v-depth^]
      [`(inner-k-depth ,l ,k) (let ((l (add1 l)))
                                (if (< l v-depth^)
                                    (begin
                                      (set! k-depth^ k)
                                      (apply-k-depth))
                                    (begin
                                      (set! k-depth^ k)
                                      (set! v-depth^ l)
                                      (apply-k-depth))
                                    #;
                                    (apply-k-depth k v) #; (apply-k-depth k l)
                                    ))]
      [`(outer-k-depth ,ls ,k)
       (begin
         (set! k-depth^ (make-inner-k-depth v-depth^ k))
         (set! ls-depth^ (cdr ls))
         (depth))
       ])))

(define depth-reg-driver
  (lambda (ls)
    (begin
      (set! ls-depth^ ls)
      (set! k-depth^ (empty-k))
      (depth))))


(define n-fact #f)
(define k-fact #f)
(define fact-inner #f)
(define v-fact #f)

(define fact
  (lambda () ; n k
    (begin
      (set! fact-inner inner-fact)
      (outer-fact))))

(define outer-fact
  (lambda () ;fact-inner n k
    (fact-inner)))

(define inner-fact
  (lambda () ;fact-inner n k
    (cond
      [(zero? n-fact) (begin
                        (set! v-fact 1)
                        (apply-k-fact))]
      [else (begin
              (set! k-fact (make-k-fact n-fact k-fact))
              (set! n-fact (sub1 n-fact))
              (inner-fact))])))

(define make-k-fact
  (lambda (n k)
    `(k-fact ,n ,k)
    #;
    (lambda (v) )))

(define apply-k-fact
  (lambda () ;k v
    (match k-fact
      [`(init-k) v-fact]
      [`(k-fact ,n ,k) (begin
                         (set! k-fact k)
                         (set! v-fact (* n v-fact))
                         (apply-k-fact))]
      )))

(define fact-reg-driver
  (lambda (n)
    (begin
      (set! n-fact n)
      (set! k-fact (empty-k))
      (fact))))

;(define n-pascal #f)
;(define k-pascal #f)
;(define p-pascal #f)
;(define v-pascal #f)

#;
(define pascal
  (lambda () ; n k
    (let ((pascal (begin
                    (make-outer-pascal))))
      (pascal pascal (make-k-let-body k-pascal)))))
#;
(define make-outer-pascal
  (lambda ()
    (lambda (pascal k)
      (begin
        (set! k-pascal k)
        (set! v-pascal (make-inner-pascal pascal n-pascal)))
      #;
      (apply-k-pascal k (make-inner-pascal pascal n-pascal)))))
#;
(define make-inner-pascal
  (lambda (pascal n)
    (lambda (m a k)
      (cond
        [(> m n) (apply-k-pascal k '())]
        [else (let ((a (+ a m)))
                (pascal pascal (make-k-let-inner m a k)))]))))
#;
(define make-k-let-inner
  (lambda (m a k)
    `(k-let-inner ,m ,a ,k)
    #;
    (lambda (v) (v (add1 m) a (make-k-pascal a k)))))
#;
(define make-k-let-body
  (lambda (k)
    `(k-let-body ,k)
    #;
    (lambda (f) (f 1 0 k))))
#;
(define make-k-pascal
  (lambda (a k)
    `(k-pascal ,a ,k)
    #;
    (lambda (v) )))

#;
(define apply-k-pascal
  (lambda () ;k v
    (match k-pascal
      [`(init-k) v-pascal]
      [`(k-let-body ,k) (v-pascal 1 0 k)]
      [`(k-let-inner ,m ,a ,k) (v-pascal (add1 m) a (make-k-pascal a k))]
      [`(k-pascal ,a ,k) (begin
                           (set! k-pascal k)
                           (set! v-pascal (cons a v-pascal))
                           (apply-k-pascal))
                         #;
                         (apply-k-pascal k (cons a v-pascal))]
      [else (apply-k-pascal k-pascal v-pascal)])))

#;
(define pascal-reg-driver
  (lambda (n)
    (begin
      (set! n-pascal n)
      (set! k-pascal (empty-k))
      (pascal))))


(define pascal-n #f)
(define pascal-k #f)
(define pascal-let #f)
(define pascal-v #f)
(define k1 #f)

(define m #f)
(define a #f)
(define k2 #f)


(define pascal
  (lambda () ; n k
    (let ((pascal*
           (lambda () ;pascal-let k1
             (begin
               (set! pascal-k k1)
               (set! pascal-v (lambda () ; m a k2
                                (cond
                                  [(> m pascal-n) (begin
                                                    (set! pascal-k k2)
                                                    (set! pascal-v '())
                                                    (apply-k-pascal))
                                                  ;(apply-k-pascal k2 '())
                                                  ]
                                  [else (let ((a1 (+ a m)))
                                          (begin
                                            (set! k1 (make-outer-k-pascal m a1 k2))
                                            #;
                                            (set! k1 (lambda (f) (f (add1 m) a (make-inner-k-pascal a k2))))
                                            (pascal-let))
                                          #;
                                          (pascal-let pascal-let (make-outer-k-pascal m a k2)))])))
               (apply-k-pascal)))))
      (begin
        (set! k1 (make-k-pascal))
        (set! pascal-let pascal*)
        (pascal*))
      #;
      (pascal* pascal* (make-k-pascal)))))

(define make-outer-k-pascal
  (lambda (m a k2)
    `(outer-k-pascal ,m ,a ,k2)
    #;
    (lambda (v) (v (add1 m) a (make-inner-k-pascal a k2)))))

(define make-inner-k-pascal
  (lambda (a k2)
    `(inner-k-pascal ,a ,k2)
    #;
    (lambda (v) (k2 (cons a v)))))


(define apply-k-pascal
  (lambda () ; pascal-k pascal-v
    (displayln pascal-k)
    (match pascal-k
      [`(init-k) pascal-v]
      [`(inner-k-pascal ,a ,k2) (begin
                                  (set! pascal-k k2)
                                  (set! pascal-v (cons a pascal-v))
                                  (apply-k-pascal))
                                ;(apply-k-pascal k2 (cons a v))
                                ]
      [`(k-pascal)
       (displayln 'pascalv)
       (displayln pascal-v)
       (begin
         (set! k2 (empty-k))
         (set! m 1)
         (set! a 0)
         (pascal-v))
       #;
       (v 1 0 pascal-k)
       ]
      [`(outer-k-pascal ,m1 ,a1 ,k21)
       (begin
         (set! k2 (make-inner-k-pascal a1 k21))
         (set! m (add1 m1))
         (set! a a1)
         (pascal-v))
       #;
       (v (add1 m) a1 (make-inner-k-pascal a1 k21))
       ]    
      #;[else (k v)]
      )))

(define make-k-pascal
  (lambda ()
    `(k-pascal)
    #;
    (lambda (v) (v 1 0 pascal-k))))

(define pascal-reg-driver
  (lambda (n)
    (begin
      (set! pascal-n n)
      (set! pascal-k (empty-k))
      (pascal))))


(define fib-n #f)
(define fib-k #f)
(define fib-v #f)
(define pc #f)
(define done #f)

(define fib
  (lambda () ; fib-n fib-k
    (displayln 'here)
    (cond
      [(and (not (negative? fib-n)) (< fib-n 2)) (begin
                                                   (set! fib-v fib-n)
                                                   (apply-k-fib)
                                                   #;
                                                   (lambda () (apply-k-fib)))]
      [else (begin
              (set! fib-k (make-outer-k-fib fib-n fib-k))
              (set! fib-n (sub1 (sub1 fib-n)))
              (fib))])))

(define make-inner-k-fib
  (lambda (n2 k)
    `(inner-k-fib ,n2 ,k)))

(define make-outer-k-fib
  (lambda (n k)
    `(outer-k-fib ,n ,k)))

(define empty-k-fib
  (lambda (jump)
    `(init-k ,jump)))

(define apply-k-fib
  (lambda () ;k v
    (match fib-k
      [`(init-k ,jumpout) (jumpout fib-v)
                          #;(set! done #t)
                          ]
      [`(inner-k-fib ,n2 ,k) (begin
                               (set! fib-k k)
                               (set! fib-v (+ fib-v n2))
                               (apply-k-fib))]
      [`(outer-k-fib ,n ,k) (begin
                              (set! fib-k (make-inner-k-fib fib-v k))
                              (set! fib-n (sub1 n))
                              (fib))]
      )))

(define fib-driver
  (lambda (n)
    (let/cc k (begin
                (set! fib-k (empty-k-fib k))
                (set! fib-n n)
                ;(set! pc fib)
                (trampoline fib)))
    #;
    (begin
      (set! fib-k (empty-k))
      (set! fib-n n)
      ;(set! pc fib)
      (trampoline fib))))

(define trampoline
  (lambda (th)
    (trampoline (th))))

#;
(define trampoline
  (lambda ()
    (if done
        fib-v
        (begin
          (pc)
          (trampoline))
        )))

(define fib-cps
  (lambda (n k)
    (lambda () (cond
            [(and (not (negative? n)) (< n 2)) (k n)]
            [else (fib-cps (sub1 (sub1 n)) (lambda (n2) (fib-cps (sub1 n) (lambda (n1) (k (+ n1 n2))))))]))))

(define trampoline-fib
  (lambda (th)
    (trampoline-fib (th))))

(call/cc (lambda (k) (trampoline-fib (fib-cps 6 k))))



(define rampoline
  (lambda (th1 th2 th3)
    (rampoline (th1) (th2) (th3))))

(define fib-ramp-driver
  (lambda (n1 n2 n3)
    (let/cc jumpout
      (rampoline
       (lambda ()
         (fib-cps n1 jumpout))
       (lambda ()
         (fib-cps n2 jumpout))
       (lambda ()
         (fib-cps n3 jumpout))))))
;(fib-cps 6 (empty-k-fn))



