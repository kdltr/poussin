(module poussin *

(import scheme
        (only (chicken base)
              define-values include error current-error-port)
        (only (chicken condition)
              handle-exceptions print-error-message))

(include "shims.scm")
(include "chicken-shims.scm")

(include "reader.scm")
(include "writer.scm")
(include "core.scm")
(include "repl.scm")

(cond-expand
      (compiling
        (let continue ()
          (handle-exceptions exn
            (begin
              (print-error-message exn (current-error-port) "Kernel error")
              (continue))
              (kernel-interpreter))))
      (else))
)
