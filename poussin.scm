(import (chicken read-syntax)
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

(define ($vau operand-tree static-environment)
  (let ((formals (car operand-tree))
        (eformal (cadr operand-tree))
        (expr (caddr operand-tree)))
    (void)))
  

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

) ; eval group
(test-exit)