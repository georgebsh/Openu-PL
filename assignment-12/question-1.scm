#lang eopl

(require "utils.scm")

;qustion 1(ג)
;adding-poly : a helper function for adding an element to the poly

(define-datatype poly poly?
  (zero-poly)
  (adding-poly
   (coeff-field number?) ; the number of the poly ex:3x^2 ( the number is 3)
   (power-field integer?) ; the power of the poly ex:3x^2 ( the power is 2)
   (rest-poly poly?)))   ; the rest of the poly

(define zero
  (lambda ()
    (zero-poly)))

(define make-poly
  (lambda (c p)
    (if (= c 0)
        (zero-poly)
        (adding-poly c p (zero-poly)))))

(define add-polys ; helper func for add-poly so we can add 2 poly's together
  (lambda (c power p)
    (if (= c 0)
        p
        (cases poly p
          (zero-poly ()
            (adding-poly c power (zero-poly)))
          (adding-poly (c2 power2 rest)
            (cond
              ((= power power2)
               (if (= (+ c c2) 0)
                   rest
                   (adding-poly (+ c c2) power rest)))
              (else
               (adding-poly c2 power2 (add-polys c power rest)))))))))

(define add-poly
  (lambda (p1 p2)
    (cases poly p1
      (zero-poly () p2)
      (adding-poly (c power rest)
        (add-polys c power (add-poly rest p2))))))

(define degree
  (lambda (p)
    (cases poly p
      (zero-poly () -1)
      (adding-poly (c power rest)
         (max power (degree rest))))))


(define coeff
  (lambda (p power)
    (cases poly p
      (zero-poly () 0)
      (adding-poly (c exp rest)
        (if ( = power exp)
            c
            (coeff rest power))))))
        
      

(define is-zero?
  (lambda (p)
    (cases poly p
      (zero-poly () #t)
      (adding-poly ( c power rest) #f))))


(define print-poly
  (lambda (p)
    (cases poly p
      (zero-poly () '())
      (adding-poly (c power rest)
        (cons (list c power )
              (print-poly rest))))))

    
(define calc-poly
  (lambda (p x)
    (cases poly p
      (zero-poly () 0)
      (adding-poly (c power rest)
         (+ ( * c (expt x power))
            (calc-poly rest x))))))


(define test-poly
  (lambda ()
    (let ((z (zero))
          (p1 (make-poly 3 4))
          (p2 (make-poly 2 1))
          (p3 (add-poly (make-poly 3 4) (make-poly 2 1)))
          (p4 (add-poly (make-poly 3 4) (make-poly -3 4))))

      (equal?? (is-zero? z) #t)
      (equal?? (print-poly z) '())

      (equal?? (coeff p1 4) 3)
      (equal?? (coeff p1 1) 0)

      (equal?? (degree p1) 4)

      (equal?? (print-poly p3) '((2 1) (3 4)))
      (equal?? (calc-poly p3 1) 5)
      (equal?? (calc-poly p3 2) 52)
      (equal?? (calc-poly p3 3) 249)

     
      (equal?? (is-zero? p4) #t)
      (equal?? (is-zero? p3) #f)

      (report-unit-tests-completed 'test-poly))))
      
      








