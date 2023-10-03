(define atom?
  (lambda (x)
    (or (number? x) (symbol? x))) )

(define general-term?
  (lambda (t)
    (or
     (atom? t)
     (general-list? t))))

(define general-list?
  (lambda (t)
    (or
     (null? t)
     (and
      (general-term? (car t))
      (general-list? (cdr t)) ) ) ) )


(define flat-term?
  (lambda (t)
    (or
     (atom? t)
     (list-of-atoms? t))) )

(define list-of-atoms?
  (lambda (t)
    (or
     (null? t)
     (and
      (atom? (car t))
      (list-of-atoms? (cdr t))) ) ) )

  
(define flat-sum-parse
  (lambda (ast s)
    (let ((first (car s))
	  (second (if (null? (cdr s)) '() (cadr s)))
	  (remaining (cddr s)) )
      
