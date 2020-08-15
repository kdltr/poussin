;; TODO cyclic list handling everywhere

($provide! (+ignore+ ignore?)
  (define-values (e ignore? d) (make-encapsulation-type))
  (define +ignore+ (e 'ignore)))

($provide! (+inert+ inert?)
  (define-values (e inert? d) (make-encapsulation-type))
  (define +inert+ (e 'inert)))

($provide! (make-environment
            environment?
            lookup
            add-bindings!)

  (define-values (E environment? D) (make-encapsulation-type))

  (define (make-environment . parents)
    (E (cons '() (map D parents))))

  (define (lookup sym env)
    (let ((binding (get-binding sym (D env))))
      (if (pair? binding)
          (cdr binding)
          (error 'lookup "unbound symbol" sym))))

  (define (get-binding sym tree)
    (let ((binding (assq sym (car tree))))
      (if (pair? binding)
          binding
	  (let lp ((parents (cdr tree)))
	    (if (null? parents)
		'()
		(let ((res (get-binding sym (car parents))))
		  (if (null? res)
		      (lp (cdr parents))
		      res)))))))

  (define (bind! env sym value)
    (let* ((tree (D env))
           (binding (assq sym (car tree))))
      (if (pair? binding)
          (set-cdr! binding value)
          (set-car! tree (cons (cons sym value)
                               (car tree))))))

  (define (add-bindings! env bindings)
    (for-each
      (lambda (binding)
        (bind! env (car binding) (cdr binding)))
      bindings)))

($provide! (operative? make-operative operate)
  (define-values (make-operative operative? D) (make-encapsulation-type))

  (define (operate operative operands env)
    ((D operative) operands env)))

(define-values (wrap applicative? unwrap) (make-encapsulation-type))

(define-values (make-comment comment? comment-contents)
  (make-encapsulation-type))

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
  (cond ((symbol? exp) (lookup exp env))
        ((pair? exp) (combine (kernel-eval (car exp) env)
                              (cdr exp)
                              env))
        (#t exp)))

(define (combine combiner operands env)
  (cond ((operative? combiner)
         (operate combiner operands env))
        ((applicative? combiner)
         (combine (unwrap combiner)
                  (map-kernel-eval operands env)
                  env))
        (#t
            (error 'combine "non-combiner in combiner position" combiner))))

(define (map-kernel-eval operands env)
  (map
    (lambda (exp) (kernel-eval exp env))
    operands))


;; ==================
;; PRIMITVE COMBINERS
;; ==================

;; TODO set-car! set-cdr! copy-es-immutable (optional)

(define core-environment (make-environment))

(define (kernel-define! symbol value)
  (add-bindings! core-environment (list (cons symbol value))))

;; helper for scheme applicatives
(define (make-scheme-applicative proc)
  (wrap (make-operative (lambda (args env) (apply proc args)))))

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

;; Booleans
(kernel-define! 'boolean? (make-scheme-predicate boolean?))

;; Equivalence under mutation
(define (n-eqv? . args)
  (if (or (null? args)
          (null? (cdr args)))
      #t
      (and (eqv? (car args) (cadr args))
           (apply n-eqv? (cdr args)))))
(kernel-define! 'eq? (make-scheme-applicative eqv?))

;; Equivalence up to mutation
(define (n-equal? . args)
  (if (or (null? args)
          (null? (cdr args)))
      #t
      (and (equal? (car args) (cadr args))
           (apply n-equal? (cdr args)))))
(kernel-define! 'equal? (make-scheme-applicative n-equal?))

;; Symbols
(kernel-define! 'symbol? (make-scheme-predicate symbol?))

;; Control
(kernel-define! 'inert? (make-scheme-predicate inert?))
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

;; Pairs and lists
(kernel-define! 'pair? (make-scheme-predicate pair?))
(kernel-define! 'null? (make-scheme-predicate null?))
(kernel-define! 'cons (make-scheme-applicative cons))

;; Environments
(kernel-define! 'environment? (make-scheme-predicate environment?))
(kernel-define! 'ignore? (make-scheme-predicate ignore?))
(kernel-define! 'eval (make-scheme-applicative kernel-eval))
(kernel-define! 'make-environment (make-scheme-applicative make-environment))

;; Environment mutation
(kernel-define! '$define!
  (make-operative
    (lambda (operand-tree env)
      (let* ((definiend (car operand-tree))
             (expression (cadr operand-tree))
             (expression-result (kernel-eval expression env))
             (new-bindings (match-formal-parameter-tree definiend expression-result '())))
        (add-bindings! env new-bindings)
        +inert+))))

;; Combiners
(kernel-define! 'operative? (make-scheme-predicate operative?))
(kernel-define! 'applicative? (make-scheme-predicate applicative?))
(kernel-define! '$vau
  (make-operative
    (lambda (operand-tree static-env)
      (let ((formals (car operand-tree))
            (eformal (cadr operand-tree))
            (expr (caddr operand-tree)))
        (make-operative
          (lambda (operands dynamic-env)
            (let ((env (make-environment static-env)))
              (add-bindings! env
                             (match-formal-parameter-tree formals operands
                               (match-formal-parameter-tree eformal dynamic-env '())))
              (kernel-eval expr env))))))))
(kernel-define! 'wrap (make-scheme-applicative wrap))
(kernel-define! 'unwrap (make-scheme-applicative unwrap))
