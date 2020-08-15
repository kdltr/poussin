(define +white-space-chars+
  (list #\tab #\newline #\vtab #\page #\return #\space))

(define +non-symbol-chars+ (append (list #\( #\) #\")
                                   +white-space-chars+))

(define +digit-chars+
  (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(define (stop? c)
  (or (eof-object? c)
      (member c +non-symbol-chars+)))

(define (digit? c) (member c +digit-chars+))
(define (white? c) (member c +white-space-chars+))

(define (kernel-read)
  (let ((c (read-char)))

    (define (next-char)
      (set! c (read-char))
      c)

    (define (skip-white-spaces)
      (if (white? c)
          (begin (next-char) (skip-white-spaces) #t)
          #f))
    
    (define (read-identifier acc)
      (if (stop? c)
          acc
          (let* ((char c))
            (next-char)
            (read-identifier (string-append acc (string char))))))

    (define (read-symbol)
      (string->symbol (read-identifier "")))
    
    (define (read-number)
      (let lp ((sum 0))
	(let* ((n (- (char->integer c) 48))
	       (new-sum (+ (* sum 10) n)))
	  (next-char)
	  (if (digit? c)
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
               (if (eqv? c #\) )
                       #t
                       (error 'read-list
                              "no end of list found"))
               (next-char)
               val))
            (#t
             (cons (top-read) (read-list)))))

    (define (read-string)
      (case c
        ((#\")
         (next-char)
         "")
        ((#\\)
         (next-char)
         (let ((char (string c)))
           (next-char)
           (string-append char (read-string))))
         (else
           (let ((char (string c)))
             (next-char)
             (string-append char (read-string))))))

    (define (read-special)
      (cond ((char=? c #\\)
             (next-char)
             (read-character))
            (#t
             (let ((char c))
               (next-char)
               (case char
                 ((#\;) (make-comment (top-read)))
		 ((#\t) #t)
		 ((#\f) #f)
                 (else (read-long-special char)))))))

    (define (read-long-special first)
      (let ((sym (string->symbol (read-identifier (string first)))))
        (case sym
          ((eof) '#!eof)
          ((inert) +inert+)
          ((ignore) +ignore+)
          (else (error 'read "unknown object" sym)))))

    (define (read-character)
      (let ((char c))
	(next-char)
	(if (stop? c)
	    char
	    (symbol->character
	     (string->symbol
	       (read-identifier (string char)))))))

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
	(else (error 'read "unknown character" sym))))

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
		 (string->symbol (read-identifier "-"))))
	    ((digit? c)
	     (read-number))
            (else
             (read-symbol))))

    (top-read)
))
