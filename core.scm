;; TODO cyclic list handling everywhere

(define-record singleton)
(define +ignore+ (make-singleton))
(define +inert+ (make-singleton))

(define (ignore? x) (eq? x +ignore+))
(define (inert? x) (eq? x +inert+))

(define-record environment bindings parents)
(define-record operative action)
(define-record applicative combiner)

(define (environment-lookup sym env)
  (define not-found (list #t))
  
  (define (lookup sym env)
    (let* ((bindings (environment-bindings env))
           (parents (environment-parents env))
           (binding (assq sym bindings)))
      (if (pair? binding)
          (cdr binding)
	  (let lp ((parents parents))
	    (if (null? parents)
		not-found
		(let ((res (lookup sym (car parents))))
		  (if (eqv? res not-found)
		      (lp (cdr parents))
		      res)))))))

  (let ((res (lookup sym env)))
    (if (eqv? res not-found)
	(error 'environment-lookup "Unbound symbol" sym)
	res)))

(define (match-formal-parameter-tree tree object result)
  (cond ((symbol? tree)
         (if (assq tree result)
             (error 'match-formal-parameter-tree "symbol occurs more than once in parameter tree" tree)
             (cons (cons tree object) result)))
        ((ignore? tree)
         result)
        ((and (null? tree) (null? object))
         result)
        ((and (pair? tree) (pair? object))
         (match-formal-parameter-tree (car tree) (car object)
           (match-formal-parameter-tree (cdr tree) (cdr object) result)))
        (#t
         (error 'match-formal-parameter-tree "malformed parameter tree" tree))))

(define (kernel-eval exp env)
  (cond ((symbol? exp) (environment-lookup exp env))
        ((pair? exp) (combine (kernel-eval (car exp) env)
                              (cdr exp)
                              env))
        (#t exp)))

(define (combine combiner operands env)
  (cond ((operative? combiner)
         (operate combiner operands env))
        ((applicative? combiner)
         (combine (applicative-combiner combiner)
                  (map-kernel-eval operands env)
                  env))
        (#t
            (error 'combine "non-combiner in combiner position" combiner))))

(define (operate operative operands env)
  ((operative-action operative) operands env))

(define (map-kernel-eval operands env)
  (map
    (lambda (exp) (kernel-eval exp env))
    operands))


;; ===================
;; PRIMITVE OPERATIVES
;; ===================

;; TODO eq? (optional)
;; TODO set-car! set-cdr! copy-es-immutable (optional)

(define core-environment (make-environment '() '()))

(define (kernel-define! symbol value)
  (environment-bindings-set!
    core-environment
    (cons (cons symbol value)
          (environment-bindings core-environment))))

;; helper for scheme applicatives
(define (make-scheme-applicative proc)
  (make-applicative (make-operative (lambda (args env) (apply proc args)))))

;; helper for scheme predicates
(define (make-scheme-predicate pred)
  (make-scheme-applicative
    (lambda args
      (let lp ((args args))
        (cond ((null? args)
               #t)
              ((pred (car args))
               (lp (cdr args)))
              (#t #f))))))

(kernel-define! 'boolean? (make-scheme-predicate boolean?))
(kernel-define! 'symbol? (make-scheme-predicate symbol?))
(kernel-define! 'inert? (make-scheme-predicate inert?))
(kernel-define! 'pair? (make-scheme-predicate pair?))
(kernel-define! 'null? (make-scheme-predicate null?))
(kernel-define! 'environment? (make-scheme-predicate environment?))
(kernel-define! 'ignore? (make-scheme-predicate ignore?))
(kernel-define! 'operative? (make-scheme-predicate operative?))
(kernel-define! 'applicative? (make-scheme-predicate applicative?))

(define (n-equal? . args)
  (if (or (null? args)
          (null? (cdr args)))
      #t
      (and (equal? (car args) (cadr args))
           (apply n-equal? (cdr args)))))

(kernel-define! 'equal? (make-scheme-applicative n-equal?))

(kernel-define! '$if
  (make-operative
    (lambda (operand-tree env)
      (let ((test (car operand-tree))
            (consequent (cadr operand-tree))
            (alternative (caddr operand-tree)))
        (let ((test-result (kernel-eval test env)))
          (cond ((not (boolean? test-result))
                 (error '$if "result of $if test is not a boolean"
                        (cons '$if operand-tree)))
                (test-result (kernel-eval consequent env))
                (#t (kernel-eval alternative env))))))))

(kernel-define! 'cons (make-scheme-applicative cons))
(kernel-define! 'eval (make-scheme-applicative kernel-eval))

(kernel-define! 'make-environment
  (make-scheme-applicative
    (lambda parents (make-environment '() parents))))

(kernel-define! '$define!
  (make-operative
    (lambda (operand-tree env)
      (let* ((definiend (car operand-tree))
             (expression (cadr operand-tree))
             (expression-result (kernel-eval expression env))
             (new-bindings (match-formal-parameter-tree definiend expression-result '())))
        (environment-bindings-set! env (append new-bindings (environment-bindings env)))
        +inert+))))


(kernel-define! '$vau
  (make-operative
    (lambda (operand-tree static-env)
      (let ((formals (car operand-tree))
            (eformal (cadr operand-tree))
            (expr (caddr operand-tree)))
        (make-operative
          (lambda (operands dynamic-env)
            (kernel-eval expr
                         (make-environment (match-formal-parameter-tree formals operands
                                             (match-formal-parameter-tree eformal dynamic-env '()))
                                           (list static-env)))))))))

(kernel-define! 'wrap (make-scheme-applicative make-applicative))
(kernel-define! 'unwrap (make-scheme-applicative applicative-combiner))
