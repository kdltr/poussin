;; TODO lists and cyclic lists
;; TODO output-only representation (operatives, applicatives…)

(define (kernel-write exp)
  (cond ((eqv? exp +ignore+)
	 (display "#ignore"))
	((eqv? exp +inert+)
	 (display "#inert"))
        ((operative? exp)
         (display "#operative"))
        ((applicative? exp)
         (display "#applicative"))
        ((boolean? exp)
         (display (if exp "#t" "#f")))
	((number? exp)
	 (display (number->string exp)))
	((symbol? exp)
	 (display (symbol->string exp)))
        ((char? exp)
         (write-character exp))
	((string? exp)
	 (write exp))
	((pair? exp)
         (display #\()
         (write-pairs exp)
         (display #\)))
	((null? exp)
	 (display "()"))
	(else
	 (display exp))))

(define (write-character char)
  (display
    (case char
      ((#\nul) "#\\null")
      ((#\alarm) "#\\alarm")
      ((#\backspace) "#\\backspace")
      ((#\tab) "#\\tab")
      ((#\newline) "#\\newline")
      ((#\return) "#\\return")
      ((#\esc) "#\\escape")
      ((#\delete) "#\\delete")
      ((#\space) "#\\space")
      (else (string-append "#\\" (string char))))))

(define (write-pairs p)
  (kernel-write (car p))
  (cond ((pair? (cdr p))
         (display #\space)
         (write-pairs (cdr p)))
        ((null? (cdr p)))
        (#t
         (display " . ")
         (kernel-write (cdr p)))))
