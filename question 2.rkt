#lang eopl
(require "utils.scm")

;; quesion 2:
(define empty-env
  (lambda ()
    (lambda (search-var)
      'variable-not-found)))

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (lambda (search-var)
      (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)))))

(define apply-env
  (lambda (env search-var)
    (env search-var)))

(define is-num?
  (lambda (e x)
    (let ((val (apply-env e x)))
      (number? val)))) 


(define run-tests
  (lambda ()
    (let ((env1 (extend-env 'a 5
                  (extend-env 'b "not a number"
                     (empty-env)))))
      (equal?? (is-num? env1 'a) #t)
      (equal?? (is-num? env1 'b) #f)
      (equal?? (is-num? env1 'w) #f)
      
      (report-unit-tests-completed 'is-num?))))


      
      
                 
                
  