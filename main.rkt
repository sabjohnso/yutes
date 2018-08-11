#lang racket/base


(provide
 prog prog1 prog2 progn
 lambda-curried define-curried make-curried
 call-with call
 cut cute
 (for-syntax syntax-symbol? syntax-number? syntax-integer? format-syntax)
 any?
 rmap
 rappend
 values->list
 values-ref
 values-set
 values-first
 values-rest
 values-take
 values-drop
 values-map)



(module core racket/base
  (require
   racket/contract
   racket/match
   (only-in racket/list take drop)
   (for-syntax racket/base racket/syntax syntax/parse))

  
  (provide
   (contract-out    
    (struct partial-application ([proc procedure?] [args list?]))
    (struct curried-procedure ([proc (and/c procedure? simple-arity?)]))
    [simple-arity? (-> procedure? boolean?)]    
    [make-curried (-> (and/c procedure? simple-arity?) curried-procedure?)]
    [call-with
	(case->
	 (-> (-> any/c (-> any/c any) any/c)) 
	 (-> any/c (-> (-> any/c any) any/c))
	 (-> any/c (-> any/c any) any/c))]
    [call (-> (-> any/c any/c) any/c any/c)]

    [syntax-symbol? (-> any/c boolean?)]
    [syntax-number? (-> any/c boolean?)]
    [syntax-integer? (-> any/c boolean?)]
    [format-syntax (-> string? syntax-symbol? syntax-symbol?)]
    
    [procedure-arity-includes>?  arity-comparison/c]
    [procedure-arity-includes<?  arity-comparison/c]
    [procedure-arity-includes>=? arity-comparison/c]
    [procedure-arity-includes<=? arity-comparison/c]

    [cut  cut/c]
    [cute cut/c]

    [rmap (->* (procedure? list?) #:rest (listof list?) list?)]
    [rappend (-> list? list? list?)]
    [any? (-> (list? boolean?) boolean?)]))

  
  (struct curried-procedure
    (proc)
    #:transparent
    #:property prop:procedure
    (lambda (this . args)
      (let* ([proc (curried-procedure-proc this)]
	     [arity (procedure-arity proc)])
	(cond [(= (length args) arity) (apply proc args)]
	      [(< (length args) arity) (partial-application this args)]
	      [(> (length args) arity)
	       (apply (apply (curried-procedure-proc this) (take args arity))
		      (drop args arity))]))))
  
  (define cut/c
    (->i ([f procedure?])
	 #:rest [xs (f)
		    (lambda (xs)
		      (and (list? xs)
			(procedure-arity-includes>=? f (length xs))))]
	 [result (f xs) (->i ()
			     #:rest [ys (lambda (ys) (procedure-arity-includes?
						      f (+ (length xs) (length ys))))]
			     [output any/c])]))
  
  (define (cut f . xs)
    (lambda ys
      (apply f (append xs ys))))

  (define (cute f . ys)
    (lambda xs
      (apply f (append xs ys))))


  (define (simple-arity? f)
    (exact-integer? (procedure-arity f)))


  (struct partial-application
    (proc args)

    #:property prop:procedure
    (lambda (this . args)
      (apply (partial-application-proc this)
	     (append (partial-application-args this)
		     args)))
    #:transparent)

  (struct cruried-procedure
    (proc)
    #:transparent
    #:property prop:procedure
    (lambda (this . args)
      (let* ([proc (curried-procedure-proc this)]
	     [arity (procedure-arity proc)])
	(cond [(= (length args) arity) (apply proc args)]
	      [(< (length args) arity) (partial-application this args)]
	      [(> (length args) arity)
	       (apply (apply (curried-procedure-proc this) (take args arity))
		      (drop args arity))]))))

  (define-syntax (lambda-curried stx)
    (syntax-parse stx
      [(_ (xs ...) es ...+)
       #'(curried-procedure (lambda (xs ...) es ...))]))

  (define-syntax (define-curried stx)
    (syntax-parse stx
      [(_ (f:id xs:id ...) es:expr ...+) #'(define f (lambda-curried (xs ...) es ...))]
      [(_ f:id e:expr) #'(define f (curried-procedure e))]))  


  (define (make-curried f)
    (curried-procedure f))

  (define call-with/c
    (case->
     (-> any/c (-> any/c any/c) any/c)
     (-> any/c (-> (-> any/c any/c) any/c))
     (-> (recursive-contract call-with/c))))
  
  (define-curried (call-with x f)
    (f x))

  (define call/c
    (case->
     (-> (-> any/c any/c) any/c any/c)
     (-> (-> any/c any/c) (-> any/c any/c))
     (-> (recursive-contract call/c))))

  (define-curried (call f x)
    (f x))

  
  (define (syntax-symbol? x)
    (and (syntax? x)
      (symbol? (syntax->datum x))))
  
  (define (syntax-number? x)
    (and (syntax? x)
      (number? (syntax->datum x))))

  (define (syntax-integer? x)
    (and (syntax? x)
      (integer? (syntax->datum x))))
  

  (define-curried (format-syntax fmt stx)
    (datum->syntax stx
      (string->symbol
       (format fmt (symbol->string (syntax->datum stx))))))

  (define arity-comparison/c
    (-> procedure? exact-nonnegative-integer? boolean?))
  
  (define (procedure-arity-includes>? proc n)
    ((lambda (f) (f f (procedure-arity proc)))
     (lambda (f arity)
       (match arity
	 [(list arity more-arities ...)
	  (if (f f arity) #t
	      (f f more-arities))]
	 ['() #f]
	 [(and (? exact-nonnegative-integer?) m) (> m n)]
	 [(arity-at-least _) #t]))))
  
  (define (procedure-arity-includes>=? proc n)
    (or (procedure-arity-includes? proc n)
      (procedure-arity-includes>? proc n)))

  (define (procedure-arity-includes<? proc n)
    ((lambda (f) (f f (procedure-arity proc)))
     (lambda (f arity)
       (match arity
	 [(list arity more-arities ...)
	  (if (f f arity) #t
	      (f f more-arities))]
	 ['() #f]
	 [(or (and (? exact-nonnegative-integer?) m) (arity-at-least m))
	  (< m n)]))))

  (define (procedure-arity-includes<=? proc n)
    (or (procedure-arity-includes? proc n)
      (procedure-arity-includes<? proc n)))


  
  (define (any? xs)
    (for/fold ([result #f])
	([x xs] #:break result)
      x))

  (define (rmap g xs . xss)
    (if (null? xss)      
	((λ (f) (f f xs '()))
	 (λ (f xs accum)
	   (if (null? xs) accum
	       (f f (cdr xs) (cons (g (car xs)) accum)))))      
	((λ (f) (f f xs xss '()))
	 (λ (f xs xss accum)
	   (if (or (null? xs) (any? (rmap null? xss))) accum
	       (f f (cdr xs) (map cdr xss)
		  (cons (apply g (car xs)
			       (map car xss)) accum)))))))

  (define (rappend xs ys)
    (if (null? xs) ys
	(rappend (cdr xs) (cons (car xs) ys))))); end of submodule core

(require
 racket/contract/base (only-in racket/function thunk)
 (submod "." core)
 (for-syntax
   racket/base racket/syntax syntax/parse
   (submod "." core)))




(define-syntax (lambda-curried stx)
  (syntax-parse stx
    [(_ (xs ...) es ...+)
     #'(curried-procedure (lambda (xs ...) es ...))]))

(define-syntax (define-curried stx)
  (syntax-parse stx
    [(_ (f:id xs:id ...) es:expr ...+) #'(define f (lambda-curried (xs ...) es ...))]
    [(_ f:id e:expr) #'(define f (curried-procedure e))]))


(define-syntax (prog stx)
  (syntax-parse stx
    [(_ es:expr ...)
     #'((thunk es ... (void)))]))

(define-syntax (prog1 stx)
  (syntax-parse stx
    [(_ e:expr es:expr ...)
     (let ([result (datum->syntax stx (gensym 'result))])
       #`((thunk
	   (let ([#,result e])
	     es ...
	     #,result))))]))

(define-syntax (prog2 stx)
  (syntax-parse stx
    [(_ e1:expr e2:expr es:expr ...)
     (let ([result (datum->syntax stx (gensym 'result))])
       #`((thunk e1
		 (let ([#,result e2])
		   es ...
		   #,result))))]))

(define-syntax (progn stx)
  (syntax-parse stx
    [(_ es:expr ...+)
     #'((thunk es ...))]))

(define-syntax values->list
  (syntax-rules ()
    [(_ expr)
     ((compose list (thunk expr)))]))

(define-syntax values-ref
  (syntax-rules ()
    [(_ expr index)
     (list-ref (values->list expr) index)]))


(define-syntax values-set
  (syntax-rules ()
    [(_ expr index value)
     (apply values
	  (list-set (values->list expr) index value))]))



(define-syntax values-first
  (syntax-rules ()
    [(_ expr)
     (car (values->list expr))]))

(define-syntax values-rest
  (syntax-rules ()
    [(_ expr)
     (apply values (cdr (values->list expr)))]))

(define-syntax values-take
  (syntax-rules ()
    [(_ expr n)
     (apply values (take (values->list expr) n))]))

(define-syntax values-drop
  (syntax-rules ()
    [(_ expr n)
     (apply values (drop (values->list expr) n))]))

(define-syntax values-map
  (syntax-rules ()
    [(_ fun expr) (apply values (map fun (values->list expr)))]))





(module+ test
  (require racket/contract rackunit)

  (contract-exercise call)
  (contract-exercise call-with)
  (contract-exercise cut)
  (contract-exercise cute)
  (contract-exercise procedure-arity-includes>? procedure-arity-includes>=?
		     procedure-arity-includes<? procedure-arity-includes<=?)


  ;; prog should accept zero or
  ;; more forms and always
  ;; returns void
  (check-true (void? (prog)))
  (check-true (void? (prog 'x)))

  ;; prog1 accepts one or more forms
  ;; and returns the result of evaluating
  ;; the first form, which must be an
  ;; expression
  (check-equal? (prog1 'x) 'x)
  (check-equal? (prog1 'x 'y) 'x)

  ;; prog2 accepts two or more forms and
  ;; returns the result of evaluating the second
  ;; form, which must be an expression
  (check-equal? (prog2 'x 'y) 'y)
  (check-equal? (prog2 'x 'y 'z) 'y)

  ;; progn accepts one or more forms
  ;; and always returns the result of evaluating
  ;; the final form which must be an expression
  (check-equal? (progn 'x) 'x)
  (check-equal? (progn 'x 'y) 'y)
  (check-equal? (progn 'x 'y 'z) 'z)

  ;; Curry the addition operator
  ;; and check the curried operator
  ;; with a number of values
  (prog (define-curried (add a b)
	  (+ a b))
	(check-equal? (add 2 3) 5)
	(check-equal? ((add 2) 3) 5)
	(let ([add2 (add 2)])
	  (check-equal? (add2 3) 5)
	  (check-equal? (add2 4) 6)))


  ;; The statement inside the thunk should fail
  ;; because + does not have a simple arity. The
  ;; usage above is correct.
  (check-exn
   exn:fail?
   (thunk (prog (define-curried add +))))





  ;; call simply applies a function to a value
  (check-equal? (call (lambda (x) (+ x x)) 4) 8)


  ;; cut is partial application with left  packing of
  ;; the arguments and cute is partial application
  ;; with right packing ofo the arguments
  (let ([axpy (lambda (a x y)
		(+ (* a x) y))])
    (check-equal?
     ((cut axpy 2 3 4))
     10)

    (check-equal?
     ((cut axpy 2 3) 4)
     10)

    (check-equal?
     ((cut axpy 2) 3 4)
     10)

    (check-equal?
     ((cut axpy) 2 3 4)
     10)

    (check-equal?
     ((cute axpy 2 3 4))
     10)

    (check-equal?
     ((cute axpy 3 4) 2)
     10)

    (check-equal?
     ((cute axpy 4) 2 3)
     10)

    (check-equal?
     ((cute axpy) 2 3 4)
     10))



  (check-equal?
   (rmap (lambda (x) (* x x)) '(1 2 3))
   '(9 4 1))

  (check-equal?
   (rmap + '(1 2 3) '(4 5 6))
   '(9 7 5))

  (check-equal?
   (rappend '(2 1) '(3 4))
   '(1 2 3 4))


  


  );; end of module test
