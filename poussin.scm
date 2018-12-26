;; TODO cyclic list handling everywhere

(import (chicken read-syntax)
        srfi-1
        test)

(define (kernel-eval exp env)
  (cond ((symbol? exp)
         (environment-lookup exp env))
        ((pair? exp)
         (call-combiner (kernel-eval (car exp) env)
                        (cdr exp)
                        env))
        (#t exp)))

(define-record ignore)
(define +ignore+ (make-ignore))
(define-record-printer (ignore _ port) (display "#!ignore"))
(set-read-syntax! 'ignore (lambda (_) '+ignore+))

(define-record inert)
(define +inert+ (make-inert))
(define-record-printer (inert _ port) (display "#!inert"))
(set-read-syntax! 'inert (lambda (_) '+inert+))

(define-record environment bindings parents)
(define-record operative formal-parameters environment-formal expression definition-environment)
(define-record applicative combiner)
(define-record foreign-operative scheme-procedure)

;; TODO depth first search of parents
(define (environment-lookup sym env)
  (let* ((bindings (environment-bindings env))
         (parents (environment-parents env))
         (binding (assq sym bindings)))
    (cond ((pair? binding)
           (cdr binding))
          ((pair? parents)
           (environment-lookup sym (car parents)))
          (#t
           (error "Unbound symbol in environment" sym)))))

(define (call-combiner combiner operand-tree env)
  (cond ((applicative? combiner)
         (call-combiner (applicative-combiner combiner)
                        (map (lambda (exp) (kernel-eval exp env)) operand-tree)
                        env))
        ((operative? combiner)
         (kernel-eval (operative-expression combiner)
                      (make-environment (match-formal-parameter-tree (operative-formal-parameters combiner) operand-tree
                                          (match-formal-parameter-tree (operative-environment-formal combiner) env '()))
                                        (list (operative-definition-environment combiner)))))
        ((foreign-operative? combiner)
         ((foreign-operative-scheme-procedure combiner) operand-tree env))
        (#t
          (error "Non-combiner in combiner position" combiner))))

(define (match-formal-parameter-tree tree object result)
  (cond ((symbol? tree)
         (when (assq tree result)
           (error "symbol occurs more than once in parameter tree" tree))
         (cons (cons tree object) result))
        ((ignore? tree)
         result)
        ((and (null? tree) (null? object))
         result)
        ((and (pair? tree) (pair? object))
         (match-formal-parameter-tree (car tree) (car object)
           (match-formal-parameter-tree (cdr tree) (cdr object) result)))
        (#t
         (error "malformed parameter tree" tree))))

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
      (every pred operand-tree))))

(define foreign-boolean? (make-foreign-predicate boolean?))

(define foreign-equal?
  (make-foreign-applicative equal?))

(define foreign-symbol? (make-foreign-predicate symbol?))

(define foreign-inert? (make-foreign-predicate inert?))

(define foreign-$if
  (make-foreign-operative
    (lambda (operand-tree env)
      (let ((test (car operand-tree))
            (consequent (cadr operand-tree))
            (alternative (caddr operand-tree)))
        (let ((test-result (kernel-eval test env)))
          (unless (boolean? test-result)
            (error "result of $if test is not a boolean" (cons '$if operand-tree)))
          (if test-result
              (kernel-eval consequent env)
              (kernel-eval alternative env)))))))

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
             (new-bindings (match-formal-parameter-tree definiend expression-result
                             (environment-bindings env))))
        (environment-bindings-set! env new-bindings)
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

(define core-environment (make-environment `((boolean? . ,foreign-boolean?)
                                             (equal? . ,foreign-equal?)
                                             (symbol? . ,foreign-symbol?)
                                             (inert? . ,foreign-inert?)
                                             ($if . ,foreign-$if)
                                             (pair? . ,foreign-pair?)
                                             (null? . ,foreign-null?)
                                             (cons . ,foreign-cons)
                                             (environment? . ,foreign-environment?)
                                             (ignore? . ,foreign-ignore?)
                                             (eval . ,foreign-eval)
                                             (make-environment . ,foreign-make-environment)
                                             ($define! . ,foreign-$define!)
                                             (operative? . ,foreign-operative?*)
                                             (applicative? . ,foreign-applicative?)
                                             ($vau . ,foreign-$vau)
                                             (wrap . ,foreign-wrap)
                                             (unwrap . ,foreign-unwrap)
                                             )
                                           '()))

(define standard-environment (make-environment '() (list core-environment)))

(define (kernel-repl)
  (let ((exp (read)))
    (unless (eof-object? exp)
      (let ((exp (case exp ((+ignore+) +ignore+) ((+inert+) +inert+) (else exp))))
        (write (kernel-eval exp standard-environment))
        (newline)
        (kernel-repl)))))


;; =====
;; TESTS
;; =====

(test-group "parameter matching"

(test "matching ignore" '() (match-formal-parameter-tree #!ignore 42 '()))
(test "matching symbol" '((a . 42)) (match-formal-parameter-tree 'a 42 '()))
(test "matching nil" '() (match-formal-parameter-tree '() '() '()))
(test-error "nil matching error" (match-formal-parameter-tree '() 42 '()))
(test "matching pair" '((a . 42) (b . 21)) (match-formal-parameter-tree '(a . b) '(42 . 21) '()))
(test "matching list" '((a . 42) (b . 21)) (match-formal-parameter-tree '(a b) '(42 21) '()))
(test-error "duplicate symbol error" (match-formal-parameter '(a (a b)) '(1 (2 3)) '()))

) ; parameter matching group

(test-group "eval"

(define empty-environment (make-environment '() '()))

(test "evaluating number" 42 (kernel-eval 42 empty-environment))
(test "evaluating strings" "foo" (kernel-eval "foo" empty-environment))
(test "evaluating #t" #t (kernel-eval #t empty-environment))
(test "evaluating #f" #f (kernel-eval #f empty-environment))

(define simple-environment (make-environment `((foo . 42)
                                               (a . 1)
                                               (b . 2)
                                               (c . 3))
                                             '()))
(define simple-child-environment (make-environment '() (list simple-environment)))

(test "evaluating bound symbol" 42 (kernel-eval 'foo simple-environment))
(test-error "evaluating unbound symbol" (kernel-eval 'bar simple-environment))
(test "evaluating bound symbol in parent environment" 42 (kernel-eval 'foo simple-child-environment))

(define simple-operative (make-operative 'x 'e 'x empty-environment))
(define simple-applicative (make-applicative simple-operative))
(define combiner-environment (make-environment (list (cons '$list simple-operative)
                                                     (cons 'list simple-applicative))
                                               (list simple-environment)))

(test "evaluating simple operative" '(a b c) (kernel-eval '($list a b c) combiner-environment))
(test "evaluating simple applicative" '(1 2 3) (kernel-eval '(list a b c) combiner-environment))


(test-group "foreign combiners"

(define foreign-environment (make-environment (list (cons '$vau foreign-$vau)
                                                    (cons 'wrap foreign-wrap))
                                              (list combiner-environment)))

(test "foreign wrap call" '(1 2 3) (kernel-eval '((wrap $list) a b c) foreign-environment))
(test "foreign $vau call" '(b a) (kernel-eval '(($vau (x y) #!ignore (list y x)) a b) foreign-environment))

) ; foreign combiners group

) ; eval group

(cond-expand (script (test-exit)) (else))