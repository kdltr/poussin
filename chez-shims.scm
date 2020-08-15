(define (make-encapsulation-type)
  (define rtd (make-record-type-descriptor 'encapsulation
                                           #f
                                           #f
                                           #f
                                           #f
                                           '#((immutable value))))
  (values (record-constructor rtd)
          (record-predicate rtd)
          (record-accessor rtd 0)))
