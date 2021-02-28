
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; 1
(define (sequence lo hi stride)
  (if (> lo hi)
      null
      (cons lo (sequence (+ lo stride) hi stride))))
; 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

; 3 
(define (list-nth-mod xs n)
  (cond
    [(< n 0) (error "list-nth-mod: negative number")]
    [(= 0 (length xs)) (error "list-nth-mod: empty list")]
    [#t (car (list-tail xs (remainder n (length xs))))]))

; 4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([p (s)])
      (cons (car p) (stream-for-n-steps (cdr p) (- n 1))))))

; 5
(define funny-number-stream
  (letrec ([f (lambda (x)
                (cons x (lambda () (let ([a (abs x)])
                                     (f (if (= 0 (remainder (+ a 1) 5))
                                       (* -1 (+ a 1))
                                       (+ a 1)))))))])
    (lambda () (f 1))))

; 6
(define dan-then-dog (lambda () (cons "dan.jpg" (lambda () (cons "dog.jpg" dan-then-dog)))))

; 7
(define (stream-add-zero s)
  (let ([p (s)])
    (lambda () (cons (cons 0 (car p)) (lambda () ((stream-add-zero (cdr p))))))))

; 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (let ([x (list-nth-mod xs n)]
                      [y (list-nth-mod ys n)])
                  (cons (cons x y) (lambda () (f (+ n 1))))))])
    (lambda () (f 0))))

; 9
(define (vector-assoc v vec)
  (letrec ([f (lambda (i)
                (if (>= i (vector-length vec))
                    #f
                    (let ([p (vector-ref vec i)])
                      (cond
                        [(not (pair? p)) (f (+ i 1))]
                        [(equal? (car p) v) p]
                        [#t (f (+ i 1))]))))])
    (f 0)))

; 10
(define (cached-assoc xs n)
  (let ([cache (make-vector n #f)]
        [pos 0])
    (lambda (v)
      (let ([p (vector-assoc v cache)])
        (if p
            p
            (let ([p (assoc v xs)])
              (if p
                  (begin
                    (vector-set! cache pos p)
                    (set! pos (remainder (+ pos 1) n))
                    p)
                  #f)))))))
