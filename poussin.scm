;; TODO cyclic list handling everywhere

(define-record singleton)
(define +ignore+ (make-singleton))
(define +inert+ (make-singleton))

(define (ignore? x) (eq? x +ignore+))
(define (inert? x) (eq? x +inert+))

(define-record environment bindings parents)
(define-record operative formal-parameters environment-formal expression definition-environment)
(define-record applicative combiner)
(define-record foreign-operative scheme-procedure)

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
	(error "Unbound symbol" sym)
	res)))

(define (match-formal-parameter-tree tree object result)
  (cond ((symbol? tree)
         (if (assq tree result)
             (error "symbol occurs more than once in parameter tree" tree)
             (cons (cons tree object) result)))
        ((ignore? tree)
         result)
        ((and (null? tree) (null? object))
         result)
        ((and (pair? tree) (pair? object))
         (match-formal-parameter-tree (car tree) (car object)
           (match-formal-parameter-tree (cdr tree) (cdr object) result)))
        (#t
         (error "malformed parameter tree" tree))))

(define (kernel-eval exp env)
  (cond ((symbol? exp) (environment-lookup exp env))
        ((pair? exp) (combine (kernel-eval (car exp) env)
                              (cdr exp)
                              env))
        (#t exp)))

(define (combine combiner operands env)
  (cond ((operative? combiner)
         (operate combiner operands env))
        ((foreign-operative? combiner)
         ((foreign-operative-scheme-procedure combiner)
          operands
          env))
        ((applicative? combiner)
         (combine (applicative-combiner combiner)
                  (map-kernel-eval operands env)
                  env))
        (#t
            (error 'combine "non-combiner in combiner position" combiner))))

(define (operate operative operands env)
  (kernel-eval (operative-expression operative)
               (make-environment (match-formal-parameter-tree (operative-formal-parameters operative) operands
                                   (match-formal-parameter-tree (operative-environment-formal operative) env '()))
                                 (list (operative-definition-environment operative)))))

(define (map-kernel-eval operands env)
  (map
    (lambda (exp) (kernel-eval exp env))
    operands))


;; ==================
;; FOREIGN OPERATIVES
;; ==================

;; foreign operatives are regular Scheme lambda that receive
;; the operand-tree non-evaluated and the dynamic environment

;; helper for foreign applicatives
(define (make-foreign-applicative proc)
  (make-applicative (make-foreign-operative proc)))

;; helper for foreign predicates
(define (make-foreign-predicate pred)
  (make-foreign-applicative
    (lambda (operand-tree env)
      (let lp ((args operand-tree))
        (cond ((null? args)
               #t)
              ((pred (car args))
               (lp (cdr args)))
              (#t #f))))))

(define foreign-boolean? (make-foreign-predicate boolean?))
(define foreign-symbol? (make-foreign-predicate symbol?))
(define foreign-inert? (make-foreign-predicate inert?))

(define (n-equal? args)
  (if (or (null? args)
          (null? (cdr args)))
      #t
      (and (equal? (car args) (cadr args))
           (n-equal? (cdr args)))))

(define foreign-equal?
  (make-foreign-applicative
    (lambda (operand-tree env)
      (n-equal? operand-tree))))

(define foreign-$if
  (make-foreign-operative
    (lambda (operand-tree env)
      (let ((test (car operand-tree))
            (consequent (cadr operand-tree))
            (alternative (caddr operand-tree)))
        (let ((test-result (kernel-eval test env)))
          (cond ((not (boolean? test-result))
                 (error '$if "result of $if test is not a boolean" (cons '$if operand-tree)))
                (test-result (kernel-eval consequent env))
                (#t (kernel-eval alternative env))))))))

(define foreign-pair? (make-foreign-predicate pair?))
(define foreign-null? (make-foreign-predicate null?))

(define foreign-cons
  (make-foreign-applicative
    (lambda (operand-tree env)
      (cons (car operand-tree) (cadr operand-tree)))))

(define foreign-environment? (make-foreign-predicate environment?))
(define foreign-ignore? (make-foreign-predicate ignore?))

(define foreign-eval
  (make-foreign-applicative
    (lambda (operand-tree env)
      (kernel-eval (car operand-tree) (cadr operand-tree)))))

(define foreign-make-environment
  (make-foreign-applicative
    (lambda (operand-tree env)
      (make-environment '() operand-tree))))

(define foreign-$define!
  (make-foreign-operative
    (lambda (operand-tree env)
      (let* ((definiend (car operand-tree))
             (expression (cadr operand-tree))
             (expression-result (kernel-eval expression env))
             (new-bindings (match-formal-parameter-tree definiend expression-result '())))
        (environment-bindings-set! env (append new-bindings (environment-bindings env)))
        +inert+))))

(define foreign-operative?* (make-foreign-predicate operative?))
(define foreign-applicative? (make-foreign-predicate applicative?))

(define foreign-$vau
  (make-foreign-operative
    (lambda (operand-tree environment)
      (let ((formals (car operand-tree))
            (eformal (cadr operand-tree))
            (expr (caddr operand-tree)))
        (make-operative formals eformal expr environment)))))

(define foreign-wrap
  (make-foreign-applicative
    (lambda (operand-tree environment)
      (make-applicative (car operand-tree)))))

(define foreign-unwrap
  (make-foreign-applicative
    (lambda (operand-tree env)
      (applicative-combiner (car operand-tree)))))

(define core-environment
  (make-environment `((boolean? . ,foreign-boolean?)
                      ;; TODO eq? (optional)
                      (equal? . ,foreign-equal?)
                      (symbol? . ,foreign-symbol?)
                      (inert? . ,foreign-inert?)
                      ($if . ,foreign-$if)
                      (pair? . ,foreign-pair?)
                      (null? . ,foreign-null?)
                      (cons . ,foreign-cons)
                      ;; TODO set-car! set-cdr! copy-es-immutable (optional)
                      (environment? . ,foreign-environment?)
                      (ignore? . ,foreign-ignore?)
                      (eval . ,foreign-eval)
                      (make-environment . ,foreign-make-environment)
                      ($define! . ,foreign-$define!) ;; (optional)
                      (operative? . ,foreign-operative?*)
                      (applicative? . ,foreign-applicative?)
                      ($vau . ,foreign-$vau)
                      (wrap . ,foreign-wrap)
                      (unwrap . ,foreign-unwrap))
                    '()))
