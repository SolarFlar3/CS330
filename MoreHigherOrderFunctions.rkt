#lang racket

;default-parms
(define (default-parms f values)
  (lambda args
    (apply f (append args
                     (list-tail values
                                (- (length values)
                                  (- (length values)
                                     (length args))))))))

;type-parms
(define (check-types types args)
  (if (empty? types)
      (values)
      (if ((first types) (first args))
          (check-types (rest types) (rest args))
          (error "wrong type")
          )))

(define (type-parms f types) ;types is from type parms function
  (lambda args
    (check-types types args)
     (apply f args)))


;new-sin2
(define (degrees-to-radians angle) (* pi (/ angle 180)))

(define (new-sin angle type)
  (cond
    ((symbol=? type 'degrees) (sin (degrees-to-radians angle)))
    ((symbol=? type 'radians) (sin angle))))

(define new-sin2 (default-parms
                   (type-parms
                    new-sin
                    (list number? symbol?))
                   (list 0 'radians)))
