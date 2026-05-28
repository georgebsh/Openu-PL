(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LET language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;
 
      (define init-env-list
        (lambda (ids inits env)
          (if (null? ids)
           env
            (let ((val (value-of (car inits) env)))
             (init-env-list
              (cdr ids)
              (cdr inits)
              (extend-env (car ids) val env))))))

     (define extend-env-list
          (lambda (ids vals env)
            (if (null? ids)
                env
                (extend-env-list
                 (cdr ids)
                 (cdr vals)
                 (extend-env (car ids) (car vals) env)))))
        
      (define check-bools
          (lambda (bools results env)
            (if (null? bools)
                #f 
                (let ((bool-val1 (expval->bool (value-of (car bools) env))))
                  (if bool-val1
                      (value-of (car results) env)
                      (check-bools (cdr bools) (cdr results) env))))))
        
      (define do-loop
        (lambda (loop-env ids steps bools results)
          (let ((result (check-bools bools results loop-env))) 
            (if result
                result
                (let ((new-vals (map (lambda (id step)
                                       (num-val (+ (expval->num (apply-env loop-env id))
                                                   (expval->num (value-of step loop-env)))))
                                     ids steps)))
                  (let ((new-env (extend-env-list ids new-vals loop-env)))   
                    (do-loop new-env ids steps bools results)))))))
  
  ;; value-of-program : Program -> ExpVal
  ;; Page: 71
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 71
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
                (let ((val1 (value-of exp1 env)))
                  (let ((bool-val1 
                         (cases expval val1
                           (bool-val (b) val1)                
                           (num-val (n) (if (zero? n) (bool-val #f)(bool-val #t))))))
                            
        (if (expval->bool bool-val1)
            (value-of exp2 env)
            (value-of exp3 env)))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        (cast-exp (typ exp1)
          (let ((val1 ( value-of exp1 env)))
            (cases type typ
              (int-type()
                (cases expval val1
                  (num-val (n) val1)
                  (bool-val (b) (if b (num-val 1) (num-val 0)))))
              (bool-type()
                (cases expval val1
                  (bool-val (b) val1)
                  (num-val (n) (if (zero? n) (bool-val #f) (bool-val #t))))))))
        
        
        (do-exp (ids inits steps bools results)
                
           (if (null? ids)
               (eopl:error 'do-exp "do loop with no variables are not allowed"))
           (if (null? bools)
               (eopl:error 'do-exp "do loop with no booleans are not allowed"))
           (let ((loop-env (init-env-list ids inits env)))
               (do-loop loop-env ids steps bools results)))
        
                  
        )))


  )

