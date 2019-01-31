#lang racket
(require (for-syntax racket/syntax syntax/parse))
(require racket/trace)

(define filter-sps
  (lambda (p ls store)
    (cond
      [(null? ls) (values ls store)]
      [else (let*-values ([(v updatedstore)
                           (if (p (car ls))
                               (values `(,(car ls))  store)
                               (values '() (append store (cons (car ls) '()))))]
                          [(rem_value rem_store) (filter-sps p (cdr ls) updatedstore)])
              (values (append v rem_value) rem_store))])))



(define filter*-sps
  (lambda (p ls store)
    (cond
      [(null? ls) (values ls store)]
      [else (let*-values (
                          [(value updatedStore)
                           (if (pair? (car ls))
                               (let-values ([(pairValues updatedStore-afterPair)
                                             (filter*-sps p (car ls) '())])
                                 (values (cons pairValues '()) (cons updatedStore-afterPair '())))
                               (if (p (car ls))
                                   (values (cons (car ls) '()) '())
                                   (values '() (cons (car ls) '()))))]
                          [(remValues remStore) (filter*-sps p (cdr ls) '())])
              (values (append value remValues) (append updatedStore remStore)))])))

(define fib-sps
  (lambda (n store)
    (cond
      [(assv n store) (values (cdr (assv n store)) store)]
      [(eqv? n 0) (values 0 (cons '(0 . 0) store))]
      [(eqv? n 1) (values 1 (cons '(1 . 1) store))]
      [else (let*-values ([(fib-n-1 fib-n-1-store) (fib-sps (sub1 n) store)]
                          [(fib-n-2 fib-n-2-store) (fib-sps (sub1 (sub1 n)) fib-n-1-store)]
                          [(result) (+ fib-n-1 fib-n-2)])
              (values result `((,n . ,result) . ,fib-n-2-store)))])))

(define-syntax and*
  (syntax-rules ()
    ((_) '#t)
    ((_ form1) form1)
    ((_ form1 forms ...) (if form1 (and* forms ...) #f))))


(define-syntax list*
  (syntax-rules ()
    ((_) (raise-syntax-error “Incorrect argument-count to list*”))
    ((_ form1) form1)
    ((_ form1 forms ...) (cons form1 (list* forms ...)))))


(define-syntax macro-list
  (syntax-rules ()
    ((_) '())
    ((_ form1) (cons form1 '()))
    ((_ form1 forms ...) (cons form1 (macro-list forms ...)))))



(define-syntax mcond
  (syntax-rules (else)
    ((_) (void))
    ((_ (else then)) then)
    ((_ (test then)) (if test
                         then
                         (void)))
    ((_ (test then) forms ...) (if test
                                   then
                                   (mcond forms ...)))))

(define-syntax macro-map
  (syntax-rules ()
    ((_ function '()) '())
    ((_ function '(form1 forms ...)) (cons (function form1) (macro-map function '(forms ...))))
    ))







