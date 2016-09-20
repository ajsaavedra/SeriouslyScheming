#lang plai

(define (repeated f count)
  (lambda (x)
    (cond
      [(> count 0) (let ([x (f x)]) ((repeated f (- count 1)) x))]
      [else x])))

(define (repeater f count)
  (lambda (x)
    (if (= count 0) x
        ((repeater f (- count 1)) (f x)))))

(define (add-one n)
  (+ 1 n))

(define (times-two n)
  (* n n))

(define (halved n)
  (/ n 2))

(define add-one-five-times
  (repeater add-one 5))

(define double-three-times
  (repeater times-two 3))

(define quarter-value
  (repeater halved 2))

(define no-change-sum
  (repeater add-one 0))

(define even-num
  (lambda (x)
    (= 0 (modulo x 2))))

(define odd-num
  (lambda (x)
    (if (= 1 (modulo x 2)) #t #f)))

(define pos-numbers
  (lambda (x)
    (> x 0)))

(define neg-numbers
  (lambda (x)
    (< x 0)))

(define (filtr procedure lst)
  (if (empty? lst)
      empty
      (cond
        [(procedure (car lst)) (cons (car lst) (filtr procedure (cdr lst)))]
        [else (filtr procedure (cdr lst))])))

(define (add-odd-nums lst)
  (foldl + 0 (filtr (lambda (n) (odd? n)) lst)))

(define (reverse-the-odds-from n procedure)
  (foldl cons '() (filter odd? (build-list (+ n 1) procedure))))

(define gcd
  (lambda (x)
    (lambda (y)
      (if (= y 0) x
         ((gcd y) (modulo x y))))))
