#lang racket

(define (check-temps1 temps)
  (if (empty? temps)
      true
      (and (cond
             ((> (first temps) 95) false)
             ((< (first temps) 5) false)
             (else (check-temps1 (rest temps)))))))
                  
(define (check-temps temps low high)
   (if (empty? temps)
      true
      (and (cond
             ((> (first temps) high) false)
             ((< (first temps) low) false)
             (else (check-temps1 (rest temps)))))))


(define (aux-convert digits mult sum)
  (if (empty? digits)
      sum
      (aux-convert (rest digits) (* mult 10) (+ (* (first digits) mult) sum))))
  
(define (convert digits)
  (aux-convert digits 1 0))
  

(define (duple lst)
  (if (empty? lst)
       empty
       (cons (list (first lst) (first lst)) (duple (rest lst)))))

(define (len lst)
  (if (empty? lst)
      0
      (+ 1 (len (rest lst)))))

(define (aux-average lst)
  (if (empty? lst)
      0
      (+ (first lst) (aux-average (rest lst)))))

(define (average lst)
   (/ (aux-average lst) (len lst)))

(define (convertCelc temp)
  (/ (- temp 32) (/ 9 5)))

(define (convertFC temps)
  (if (empty? temps)
      empty
      (cons (convertCelc (first temps)) (convertFC (rest temps)))))

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


(define (get-nth lst n)
  (if (= n 0)
      (first lst)
      (get-nth (rest lst) (- n 1))))

(define (aux-find-item inc lst target)
  (if (empty? lst)
      -1
      (if (= (first lst) target)
          inc
          (aux-find-item (+ inc 1) (rest lst) target))))

(define (find-item lst target)
  (aux-find-item 0 lst target))
  
      
          
      

(check-temps1 (list 80 92 56))
(check-temps1 (list 80 99 56))
(check-temps (list 80 92 56) 5 95)
(check-temps (list 80 99 56) 5 95)
(convert (list 1 2 3))
(duple (list 1 2 3))
(average (list 1 2 3 4))
(convertFC (list 32 50 212))
(eliminate-larger (list 1 2 3 9 4 5))
(get-nth (list 1 2 3 4) 2)
(find-item (list 1 2 3 4) 3)
(find-item (list 1 2 3 4) 42)
