(define env0
 `(  (+ . ,+) (- . ,-) (* . ,*) (/ . ,/)
	 (apply	. ,apply)
	 (assq	. ,assq)
	 (car	. ,car)
	 (cdr	. ,cdr)
	 (cons	. ,cons)
	 (eq?	. ,eq?)
	 (list	. ,list)
	 (map	. ,map)
	 (memv	. ,memv)
	 (null? . ,null?)
	 (pair? . ,pair?)
	 (read	. ,read)
	 (symbol?	. ,symbol?)
	 (call/cc	. ,call/cc)
	 (set-car!	. ,set-car!)
	 (set-cdr!	. ,set-cdr!)))

(define ext-env
 (lambda (vars vals env)
  (cond
   ((null? vars) env)
   ((symbol? vars) (cons (cons vars vals) env))
   (else (cons
		  (cons (car vars) (car vals))
		  (ext-env (cdr vars) (cdr vals) env))))))
(define lookup
 (lambda (var env)
  (cdr (assq var env))))
(define assign
 (lambda (var val env)
  (set-cdr! (assq var env) val)))
(define interp
 (lambda (expr env)
  (cond
   ((symbol? expr) (lookup expr env))
   ((pair? expr)
	(case (car expr)
	 ((quote) (cadr expr))
	 ((lambda)
	  (lambda vals
	   (let ((env (ext-env (cadr expr) vals env)))
		(let loop ((exprs (cddr expr)))
		 (if (null? (cdr exprs))
		     (interp (car exprs) env)
			 (begin (interp (car exprs) env)
			        (loop (cdr exprs))))))))
	 ((if)
	  (if (interp (cadr expr) env)
	      (interp (caddr expr) env)
		  (interp (cadddr expr) env)))
	 ((set!)
	  (assign (cadr expr)
	          (interp (caddr expr) env)
			  env))
	 (else
	  (apply (interp (car expr) env)
	         (map (lambda (x) (interp x env)) (cdr expr))))))
	(else expr))))
(define meta
 (lambda (expr)
  (interp expr env0)))
