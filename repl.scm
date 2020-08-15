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
    (cond ((eof-object? exp))
          (#t
              (kernel-write (kernel-eval exp env))
              (newline)
              (kernel-repl env)))))

(define ground-environment (make-environment core-environment))
(kernel-load "../kernel-lib/lib1.k" ground-environment)

(define (kernel-interpreter)
  (kernel-repl ground-environment))
