(test-group "cyclic list split"

(test "null" '(() 0 () 0) (split '()))

(test "non-cyclic" '((1 2 3) 3 () 0) (split '(1 2 3)))

(test "improper" '((1 2 . 3) 2 () 0) (split '(1 2 . 3)))

(define cl0 (list 1))
(set-cdr! cl0 cl0)
(test "mono cyclic" `(() 0 ,cl0 1) (split cl0))

(define cl1 (list 1 2 3))
(set-cdr! (list-tail cl1 2) cl1)
(test "simple cyclic" `(() 0 ,cl1 3) (split cl1))

(define cl2 (list 1 2 3))
(define cl2-start (list-tail cl2 2))
(set-cdr! cl2-start cl2-start)
(test "complex cyclic" `((1 2) 2 ,cl2-start 1) (split cl2))

) ; cyclic list split


(test-group "equal? procedure"

;; TOOD base cases

(define cl1 (list 1 2 3))
(set-cdr! (list-tail cl1 2) (list-tail cl1 2))

(define cl2 (list 1 2 3 3))
(set-cdr! (list-tail cl2 3) (list-tail cl2 3))

(test "cyclic" #t (kernel-equal? cl1 cl2))

) ; equal? procedure
