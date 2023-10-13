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

;; Herein, we build on the idea in ss-passing.scm to try and
;; build a recursive-descent arithmetic expressions parser
;; which handles "MDAS" rule, including using parentheses
;; to override higher-precedence of multiplication-division.

;; <SS> IS THE START SYMBOL.
;; FORMERLY:
;; <SS> ::=  <P> | (<P> . <RSL>)
;; <RSL> ::= ( <SUMOP> . <RSL1> )
;;
;; BUT NOW I'LL TRY A REVISION TO TRY AND SIMPLIFY THE PARSING:
;; <SS> ::=  <P> | ( <P> . ( <SUMOP> . <RSL1> ) ) ??? I'M GOING TO TRY THIS FOR NOW, SEE IF I CAN AVOID RSL PRODUCTION ALTOGETHER...
;; <RSL1> ::=  ( <P> ) | ( <P> . ( <SUMOP> . <RSL1> ) )
;;
;; I'll try to simplify further:
;; <SS> ::=  <P> | ( <P> . ( <SUMOP> . <SS> ) ) ??? I'M GOING TO TRY THIS FOR NOW, SEE IF I CAN AVOID RSL PRODUCTION ALTOGETHER...

(define simple-sum-parse
  (lambda (s)
    (cond ((and (null? (cdr s))
                (not (aop? (car s))) ) (product s) )
          ( #t (let* ((p-parse (product s)) ;; parse out initial product, a <P>
                      (tkns    (cadr p-parse)) ;; remaining tokens from parsing first <P> item
                      (rator   (car tkns)) ;; operator is first token, should satisfy "sumop?" right?
                      (rand    (car p-parse))
                      (rsl-parse (simple-sum-parse (cdr tkns))) ;; parse-out the <RSL> bit
                      (rand2   (car rsl-parse))    ;; second operand, from the parsing
                      (f-tkns  (cadr rsl-parse)) )     ;; remaining tokens to send back to caller
                 (list (list rator rand rand2) f-tkns) ) )
	  )
    )
  )

;; For now, this is a stand-in version of product:
(define product
  (lambda (s)
    (list (car s) (cdr s))) )

;; <P> ::=  <F> | ( <F> . <RPL> )
;; <RPL> ::= ( <MULOP> . <RPL1> )
;; <RPL1> ::= ( <F> ) | ( <F> . ( <MULOP> . <RPL1> ) )
;;
;; <F> ::=  <A> | <LB> <SS> <RB>
