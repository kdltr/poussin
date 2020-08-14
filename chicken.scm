(module poussin *

(import scheme)
(import (only (chicken base) include error define-record))

;; TODO remove
(import (chicken base) (chicken condition) utf8 utf8-srfi-14 utf8-case-map unicode-char-sets)

(include "reader.scm")
(include "writer.scm")
(include "poussin.scm")
(include "repl.scm")

(kernel-repl ground-environment)
)
