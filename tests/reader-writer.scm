(define (test-read expected string)
  (test (conc "reading " string " (EOF ending)")
        expected
        (with-input-from-string string kernel-read))
  (test (conc "reading " string " (newline ending)")
	expected
	(with-input-from-string (conc string #\newline) kernel-read))
  (test (conc "writing " string)
        string
        (with-output-to-string (lambda () (kernel-write expected)))))

(test-group "booleans"
	    (test-read #t "#t")
	    (test-read #f "#f"))

(test-group "special objects"
            (test-read +inert+ "#inert")
            (test-read +ignore+ "#ignore")
            (test-read +undefined+ "#undefined"))

(test-group "some integers"
	    (for-each
	     (lambda (i)
	       (test-read i (number->string i)))
	     (iota 2001 -1000 1)))

(test-group "some characters"
	    (for-each
	     (lambda (i)
	       (let ((c (integer->char i)))
		 (test-read c (with-output-to-string (lambda () (write c))))))
	     ;; printable ASCII
	     (iota 94 33)))

(test-group "special characters"
	    (test-read #\null "#\\null")
	    (test-read #\alarm "#\\alarm")
	    (test-read #\backspace "#\\backspace")
	    (test-read #\tab "#\\tab")
	    (test-read #\newline "#\\newline")
	    (test-read #\return "#\\return")
	    (test-read #\escape "#\\escape")
	    (test-read #\delete "#\\delete")
	    (test-read #\space "#\\space"))

(test-group "symbols"
            (test-read 'foo "foo")
            (test-read '|æðœù| "æðœù"))

(test-group "lists"
            (test-read '() "()")
            (test-read '(1) "(1)")
            (test-read '(1 2 3) "(1 2 3)")
            (test-read '(+ (* a x) (* b y) c)
                       "(+ (* a x) (* b y) c)")
            (test-read '(a . b) "(a . b)")
            (test-read '(a b c . d) "(a b c . d)")
            #;(test-read '($vau ((car . cdr)) e car) "($vau ((car . cdr)) e car)")
            (test-read '((a . b) c) "((a . b) c)")
)

(test-group "strings"
            (test-read "foo" "\"foo\"")
            (test-read "æðœù" "\"æðœù\""))
