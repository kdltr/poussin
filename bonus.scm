(kernel-define! '$scheme-eval
  (make-operative
    (lambda (operands env)
      (eval (car operands)))))
