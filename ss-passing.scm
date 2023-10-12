(define atom?
  (lambda (x)
    (or (number? x) (symbol? x))) )

;; NEWLY ADDED "OPERATOR PREDICATE-TEST":
(define aop?
  (lambda (x)
    (or (sumop? x) (mulop? x)) ) )

(define sumop?
  (lambda (x)
    (and (symbol? x) (or (eq? x '+) (eq? x '-)))))

(define mulop?
  (lambda (x)
    (and (symbol? x) (or (eq? x '*) (eq? x '/))) ) )

(define lbrack?
  (lambda (x)
    (and (char? x) (eq? x '#\( )) ) )
(define rbrack?
  (lambda (x)
    (and (char? x) (eq? x '#\) )) ) )

;; <SS> ::=  <A> | <RS>
;;
;; <RS> ::=  <A> . ( <SUMOP> . <RL> )
;;
;; <RL> ::=  <A> . '()
;;         | <A> . ( <SUMOP> . <RL> )
;;   parse-tree should be:
;;           <A>   "bare atom"?
;;         | <SUMOP> . ( <A> . ( <A> . '() ) )       == (<SUMOP> <A> <A>)
;;         | <SUMOP> . ( <A> . ( <SUMOP> . (...) ) ) == (<SUMOP> <A> (<SUMOP> <A> (...)) )

;; asumption:  s ::= <list-of-tokens> ?
;; returns:  ( <result-of-parse> <list-of-tokens> ), i.e., a parse-tree, and then a list stored in the cadr...
(define simple-sum-parse
  (lambda (s)
    (if (and (null? (cdr s)) (not (aop? (car s)))) (list (car s) (cdr s))
        (rest-of-sum-parse s) ) ) )

;; This seems to get us part of the way there,
;; but it does not check (sumop? rator)
(define rest-of-sum-parse
  (lambda (s)
    (let* ((rand (car s))
           (rator (cadr s))
           (rs-parse (rest-of-sum-list-parse (cddr s)))
           (parsed-rsl (car rs-parse))
           (rem-tokens (cadr rs-parse))
           )
      (list (list rator rand parsed-rsl) rem-tokens)
      ) ) )
           
(define rest-of-sum-list-parse
  (lambda (s)
    (cond
     ((null? (cdr s)) (list (car s) (cdr s)))
     (#t (let* ((rand (car s))
                (rator (cadr s))
                (rsl-parsed (rest-of-sum-list-parse (cddr s)))
                (rand2 (car rsl-parsed))
                (rem-tokens (cadr rsl-parsed))
                )
           (list (list rator rand rand2) rem-tokens) )
         )
     ) ) )
