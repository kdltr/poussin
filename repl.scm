(import (chicken condition) poussin)

(define (kernel-repl env)
  (let ((exp (kernel-read)))
    (unless (eof-object? exp)
      (handle-exceptions exn (print-error-message exn (current-error-port) "Kernel error")
        (kernel-write (kernel-eval exp env))
        (newline))
      (kernel-repl env))))

(define ground-environment
  (make-environment '() (list core-environment)))

(kernel-load "lib1.k" ground-environment)
(cond-expand
      ((or compiling chicken-script)
       (kernel-repl ground-environment))
      (else))
