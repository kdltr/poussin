(define-syntax $provide!
  (syntax-rules ()
    ((_ (names ...) body ...)
     (define-values (names ...)
       (let ()
         body ...
         (values names ...))))))
