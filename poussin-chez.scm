(include "shims.scm")
(include "chez-shims.scm")

(include "reader.scm")
(include "writer.scm")
(include "core.scm")
(include "repl.scm")

(include "bonus.scm")

(let continue ()
  (with-exception-handler
    (lambda (ex)
      (display "Kernel error: (")
      (display (condition-who ex))
      (display ") ")
      (display (condition-message ex))
      (display ": ")
      (display (condition-irritants ex))
      (newline)
      (continue))
    kernel-interpreter))
