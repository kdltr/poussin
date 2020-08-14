(define (kernel-load file env)
  (with-input-from-file file
    (lambda ()
      (let lp ()
        (let ((exp (kernel-read)))
          (if (eof-object? exp)
              #t
              (begin
                (kernel-eval exp env)
                (lp))))))))

(define (kernel-repl env)
  (let ((exp (kernel-read)))
    (unless (eof-object? exp)
      (handle-exceptions exn (print-error-message exn (current-error-port) "Kernel error")
        (kernel-write (kernel-eval exp env))
        (newline))
      (kernel-repl env))))

(define ground-environment
  (make-environment '() (list core-environment)))
