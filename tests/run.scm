(import (chicken load) (chicken port) srfi-1 test utf8)

(load-relative "../poussin.scm")

(test-group "cyclic lists"
	    (load-relative "cycle.scm"))

(test-group "reader"
	    (load-relative "reader.scm"))

(test-group "writer"
	    (load-relative "writer.scm"))

(test-group "parameter matching"

(test "matching ignore" '() (match-formal-parameter-tree #!ignore 42 '()))
(test "matching symbol" '((a . 42)) (match-formal-parameter-tree 'a 42 '()))
(test "matching nil" '() (match-formal-parameter-tree '() '() '()))
(test-error "nil matching error" (match-formal-parameter-tree '() 42 '()))
(test "matching pair" '((a . 42) (b . 21)) (match-formal-parameter-tree '(a . b) '(42 . 21) '()))
(test "matching list" '((a . 42) (b . 21)) (match-formal-parameter-tree '(a b) '(42 21) '()))
(test-error "duplicate symbol error" (match-formal-parameter '(a (a b)) '(1 (2 3)) '()))

) ; parameter matching group

(test-group "eval"

(define empty-environment (make-environment '() '()))

(test "evaluating number" 42 (kernel-eval 42 empty-environment))
(test "evaluating strings" "foo" (kernel-eval "foo" empty-environment))
(test "evaluating #t" #t (kernel-eval #t empty-environment))
(test "evaluating #f" #f (kernel-eval #f empty-environment))

(define simple-environment (make-environment `((foo . 42)
                                               (a . 1)
                                               (b . 2)
                                               (c . 3))
                                             '()))

(define simple-child-environment (make-environment '() (list simple-environment)))
(define multi-parent-environment (make-environment '() (list empty-environment
							     simple-child-environment)))

(test "evaluating bound symbol" 42 (kernel-eval 'foo simple-environment))
(test-error "evaluating unbound symbol" (kernel-eval 'bar simple-environment))
(test "evaluating bound symbol in parent environment" 42 (kernel-eval 'foo simple-child-environment))
(test "evaluating bould symbol deep in ancestors" 42 (kernel-eval 'foo multi-parent-environment))

(define simple-operative (make-operative 'x 'e 'x empty-environment))
(define simple-applicative (make-applicative simple-operative))
(define combiner-environment (make-environment (list (cons '$list simple-operative)
                                                     (cons 'list simple-applicative))
                                               (list simple-environment)))

(test "evaluating simple operative" '(a b c) (kernel-eval '($list a b c) combiner-environment))
(test "evaluating simple applicative" '(1 2 3) (kernel-eval '(list a b c) combiner-environment))


(test-group "foreign combiners"

(define foreign-environment (make-environment (list (cons '$vau foreign-$vau)
                                                    (cons 'wrap foreign-wrap))
                                              (list combiner-environment)))

(test "foreign wrap call" '(1 2 3) (kernel-eval '((wrap $list) a b c) foreign-environment))
(test "foreign $vau call" '(b a) (kernel-eval '(($vau (x y) #!ignore (list y x)) a b) foreign-environment))

) ; foreign combiners group

) ; eval group

(test-exit)
