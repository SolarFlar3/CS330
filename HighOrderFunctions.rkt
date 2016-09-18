#lang racket

(define (convertCelc temp)
  (/ (- temp 32) (/ 9 5)))

(define (convertFC temps)
  (map convertCelc temps))

(define (check-temp1 temp)
  (cond
    ((> temp 95) false)
    ((< temp 4) false)
    (else true)))

(define (check-temps1 temps)
  (andmap check-temp1 temps))

(define (check-temps temps low high)
  (andmap (lambda (x)
            (cond
              ((> x high) false)
              ((< x low) false)
              (else true))) temps))

(define (next-element-bigger digits mult)
  (if (empty? digits)
      (list 0)
      (append (list (* (first digits) mult)) (next-element-bigger (rest digits) (* mult 10)))))
      
(define (convert digits)
  (foldr + 0 (next-element-bigger digits 1)))

(define (double item)
  (cons item (list item)))

(define (duple lst)
  (map double lst))

(define (average lst)
  (/ (foldr + 0.0 lst) (length lst)))

(define (smaller-than-list? num lst)
  (if (empty? lst)
      true
      (and (cond
             ((> num (first lst)) false)
             (else (smaller-than-list? num (rest lst)))))))

(define (eliminate-larger lst)
  (if (empty? lst)
      empty
      (if (smaller-than-list? (first lst) (rest lst))
          (cons (first lst) (eliminate-larger (rest lst)))
          (eliminate-larger (rest lst)))))