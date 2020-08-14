(module poussin *

(import scheme)
(import (only (chicken base) include error define-record))

;; TODO remove
(import (chicken base) (chicken condition) (srfi 1) utf8 utf8-srfi-14 utf8-case-map unicode-char-sets)

(include "cycle.scm")
(include "reader.scm")
(include "writer.scm")
(include "core.scm")
(include "repl.scm")

(kernel-load "../kernel-lib/lib1.k" ground-environment)

(cond-expand
      (compiling
       (kernel-repl ground-environment))
      (else))
)
