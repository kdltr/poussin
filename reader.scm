(define char-set:non-symbol (char-set-union char-set:white-space
                                            (char-set #\( #\) #\")))

(define (stop? c)
  (or (eof-object? c)
      (char-set-contains? char-set:non-symbol c)))

(define (digit? c)
  (char-set-contains? char-set:digit c))


(define (string-ci->symbol str)
  (string->symbol (utf8-string-downcase str)))

(define (kernel-read)
  (let ((c (read-char)))

    (define (next-char)
      (set! c (read-char))
      c)

    (define (skip-white-spaces)
      (if (char-set-contains? char-set:white-space c)
          (begin (next-char) (skip-white-spaces) #t)
          #f))
    
    (define (read-identifier acc)
      (if (stop? c)
          (conc acc "") ;; convert to string
          (let* ((char c))
            (next-char)
            (read-identifier (conc acc char)))))

    (define (read-symbol)
      (string-ci->symbol (read-identifier "")))
    
    (define (read-number)
      (let lp ((sum 0))
	(let* ((n (- (char->integer c) 48))
	       (new-sum (+ (* sum 10) n)))
	  (next-char)
	  (if (char-set-contains? char-set:digit c)
	      (lp new-sum)
	      new-sum))))

    (define (read-list)
      (skip-white-spaces)
      (cond ((char=? #\) c)
             (begin (next-char) '()))
            ((char=? #\. c)
             (next-char)
             (let ((val (top-read)))
               (skip-white-spaces)
               (assert (eqv? c #\) ))
               (next-char)
               val))
            (#t
             (cons (top-read) (read-list)))))

    (define (read-string)
      (case c
	((#\") (begin (next-char) ""))
	((#\\)
	 (next-char)
	 (conc c (begin (next-char) (read-string))))
	(else
	 (conc c (begin (next-char) (read-string))))))

    (define (read-special)
      (cond ((char=? c #\\)
             (next-char)
             (read-character))
            (#t
	     (let ((sym (read-symbol)))
               (case sym
		 ((t) #t)
		 ((f) #f)
                 ((eof) '#!eof)
                 ((inert) +inert+)
                 ((ignore) +ignore+)
                 (else (error "unknown object" sym)))))))

    (define (read-character)
      (let ((char c))
	(next-char)
	(if (stop? c)
	    char
	    (symbol->character
	     (string-ci->symbol
	       (read-identifier char))))))

    (define (symbol->character sym)
      (case sym
	((null) #\nul)
	((alarm) #\alarm)
	((backspace) #\backspace)
	((tab) #\tab)
	((newline) #\newline)
	((return) #\return)
	((escape) #\esc)
	((delete) #\delete)
	((space) #\space)
	(else (error "unknown character" sym))))

    (define (top-read)
      (skip-white-spaces)
      (cond ((eof-object? c) c)
	    ((char=? #\( c)
	     (next-char)
             (read-list))
	    ((char=? #\# c)
	     (next-char)
	     (read-special))
	    ((char=? #\" c)
	     (next-char)
	     (read-string))
	    ((char=? #\- c)
	     (next-char)
	     (if (digit? c)
		 (- (read-number))
		 (string-ci->symbol (read-identifier #\-))))
	    ((digit? c)
	     (read-number))
            (else
             (read-symbol))))

    (top-read)
))
