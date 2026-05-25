#lang scheme
(require "utils.scm")
;first question(א) : takes 2 arrays and adds them together and returns the appeneded array
(define ( my_append list1 list2)
  (if(null? list1)
     list2
  (cons (car list1) (my_append (cdr list1) list2))))


;first question(ב) : takes 2 arrays and adds them together and returns the appeneded array with foldr
(define (my_append_fr list1 list2)
  (if(null? list1)
     list2
     (foldr cons list2 list1)))

(equal?? (my_append '( "a" "b" "c") '("x" "y" "z")) '("a" "b" "c" "x" "y" "z"))
(equal?? (my_append '() '("x" "y")) '("x" "y"))
(equal?? (my_append '("a" "b") '()) '("a" "b"))
(equal?? (my_append '() '()) '())
(report-unit-tests-completed 'my_append)
(equal?? (my_append_fr '( "a" "b" "c") '("x" "y" "z")) '("a" "b" "c" "x" "y" "z"))
(equal?? (my_append_fr '() '("x" "y")) '("x" "y"))
(equal?? (my_append_fr '("a" "b") '()) '("a" "b"))
(equal?? (my_append_fr '() '()) '())
(report-unit-tests-completed 'my_append_fr)




;second question:returns true or false for a random procedure of our choosing  , x is a pointer and curr is a an empty list at the start
(define (filter proc list)
  (foldr(λ ( x curr)
          (if ( proc x)
              (cons x curr)
              curr))
        '()
        list))

(equal?? (filter even? '(1 2 3 4 5 6)) '(2 4 6))
(equal?? (filter number? '(a 1 b 2 "c")) '(1 2))
(equal?? (filter symbol? '()) '()) 
(report-unit-tests-completed 'filter)






;third question: returns every subset of a given list and it returns every subset as a list in itself
(define (powerset list)
  (if (null? list)
      '(())
      (let ((rest (powerset(cdr list))))
        (append (map(λ (sub) (cons (car list) sub))
                    rest)
                     rest))))
  
(equal?? (powerset '(1 2)) '((1 2) (1) (2) ()))
(equal?? (powerset '(3 2 1)) '((3 2 1) (3 2) (3 1) (3) (2 1) (2) (1) ()))
(report-unit-tests-completed 'powerset)



