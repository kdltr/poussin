;; Numeric procedures

(kernel-define! '+   (make-scheme-applicative +))
(kernel-define! '-   (make-scheme-applicative -))
(kernel-define! '>?  (make-scheme-applicative >))
(kernel-define! '<?  (make-scheme-applicative <))
(kernel-define! '>=? (make-scheme-applicative >=))
(kernel-define! '<=? (make-scheme-applicative <=))
(kernel-define! '=?  (make-scheme-applicative =))


;; Encapsulations

(kernel-define! 'make-encapsulation-type
  (wrap (make-operative
    (lambda (operands env)
      (call-with-values
        (lambda () (make-encapsulation-type))
        (lambda res (map make-scheme-applicative res)))))))


;; Temporary loading applicative

(kernel-define! 'load
  (wrap (make-operative (lambda (operands env)
                          (kernel-load (car operands) env)))))


;; Access to Scheme

(kernel-define! '$scheme-eval
  (make-operative
    (lambda (operands env)
      (eval (car operands)))))
