(import (srfi 99))

(define (make-encapsulation-type)
  (define rtd (make-rtd 'encapsulation '#((immutable value))))
  (values (rtd-constructor rtd)
          (rtd-predicate rtd)
          (rtd-accessor rtd 'value)))
