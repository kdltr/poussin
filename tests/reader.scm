(define (test-read expected string)
  (test string expected (with-input-from-string string kernel-read))
  (test (conc string " (newline ending)")
	expected
	(with-input-from-string (conc string #\newline) kernel-read)))

(test-group "booleans"
	    (test-read #t "#t")
	    (test-read #f "#f"))

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
