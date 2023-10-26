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
;;
;; Between trying to think through the meaning of the BNF
;; grammar, and writing the parsing functions,
;; the above simplification emerged.

;; Parses list-of-tokens s
;; Yields the result of the immediate parse, and the remaining tokens,
;; which must be returned to the caller.
;; The list-of-remaining-tokens has to be returned to a caller and
;; also passed in because of the mutually-recursive design of
;; the parsing procedures.

(define sum-parse
  (lambda (s)
    (cond ((and (null? (cdr s))
                (not (aop? (car s))) ) (product s) )
          ( #t (let* ((p-parse (product s)) ;; parse out initial product, a <P>
                      (tkns    (cadr p-parse)) ;; remaining tokens from parsing first <P> item
                      ;; N.B. it's possible that the parsing of a product,
                      ;; the result of which is bound to name p-parse,
                      ;; leaves nothing more to parse!
                      (rator   (if (null? tkns) '()
                                   (car tkns)) ) )
                 ;; operator is first token, should satisfy "sumop?" right?
                 (if (sumop? rator)
                     (let* ((rand    (car p-parse))
                            (ss-parse (sum-parse (cdr tkns))) ;; parse-out the <SS>, recursively
                            (rand2   (car ss-parse))          ;; second operand, from the parsing
                            (f-tkns  (cadr ss-parse)) )       ;; remaining tokens to send back to caller
                       (list (list rator rand rand2) f-tkns) )
                     ;; This is what the rest of parsing means
                     ;; if the operator is NOT for addition/subtraction:
                     (list (car p-parse) tkns) 
                     )
                 )
          )
    )
    )
  )

;; I shall try to re-work the grammar for <P> in a similar
;; fashion to what was done in the grammer for <SS> :
;; <P> ::=  <F> | ( <F> . <RPL> )
;; <RPL> ::= ( <MULOP> . <RPL1> )
;; <RPL1> ::= ( <F> ) | ( <F> . ( <MULOP> . <RPL1> ) )

;; Revised grammar production for products, for <P>'s :
;; <P> ::=  <F> | ( <F> . ( <MULOP> . <P> ) )
;; <F> ::=  <A> | <LB> <SS> <RB>

(define product
  (lambda (s)
    (cond ((and (null? (cdr s))
                (not (aop? (car s))) ) (factor s))
          ( #t (let* ((f-parse (factor s))
                      (tkns    (cadr f-parse))
                      ;; N.B. it's possible that the parsing of a factor,
                      ;; result-of-which is bound to name f-parse,
                      ;; leaves nothing more to parse!
                      (rator   (if (null? tkns) '() (car tkns))))
                 (if (mulop? rator)
                     (let* ((rand    (car f-parse))
                            (prd-parse (product (cdr tkns))) ;; recursively parses a <P>, a product
                            (p-tkns  (cadr prd-parse))
                            (rand2   (car prd-parse)) )
                       (list (list rator rand rand2) p-tkns) )
                     ;; this is what the rest of parsing means
                     ;; if the operator is NOT for multiplication:
                     (list (car f-parse) tkns)
                     )
                 )
            )
          )
    )
  )
  

;;
;; <F> ::=  <A> | <LB> <SS> <RB>
(define factor
  (lambda (s)
    (cond ((and (atom? (car s))
                (not (lbrack? (car s)))
                (not (rbrack? (car s)))) (list (car s) (cdr s)))
          ( #t (let* ((lb? (lbrack? (car s)))
                      (sub-parse (sum-parse (cdr s)))
                      (sub-sum (car sub-parse))
                      (tkns (cadr sub-parse))
                      (rb? (rbrack? (car tkns))) )
                 (if (and lb? rb?)
                     (list sub-sum (cdr tkns))
                     'error) ) )
          )
    )
  )

;; Thin wrapper for the top-level parsing procedure
;; which strips away the returned list-of-remaining-tokens
(define parse-expr
  (lambda (s)
    (car (sum-parse s)) ))

;; "Postorder traversal" of parse-trees to produce RPN "programs"
;; from arithmetic expressions:
;;
;; A "parse tree" structure is defined thus:
;;   <PT> ::=  <A> | ( <AOP> <PT> <PT> )
(define rpn-program
  (lambda (t)
    (if (atom? t) t
        (let* ((lrslt (rpn-program (cadr t)))
               (rrslt (rpn-program (caddr t))))
          (list lrslt rrslt (car t)) )
        ) ) )

;; This derives a list of operands-and-operators,
;; arranged into an RPN program, like a forth evaluation,
;; except it's in reverse:
;; it accepts a parse-tree and the list which is
;; the "rpn program generated thus far."
(define rpn-ify
  (lambda (t p)
    (if (atom? t) (cons t p)
        (let* ((lp (rpn-ify (cadr t) p))
               (rp (rpn-ify (caddr t) lp)) )
          (cons (car t) rp) ) ) ) )

;; Thin wrapper around rpn-ify to hand it the
;; empty list which is the start of the
;; program-to-be-built:
(define rpn-of
  (lambda (r)
    (reverse (rpn-ify r '())) ) )


(define s3 '( #\( 1 + 3 #\) * 6 ) )
(define s2 '( 1 + 2 + 3))
(define s4 '( 6 * #\( 1 + 3 #\) ) )
(define s5 '( 1 + 3 * 6 ) )
(define s6 '( #\( 4 - #\( 8 + 1 + 1 #\) #\) / 4 ) ) 

;; How do we push operands onto a stack,
;; and then start using operators to consume
;; values from the stack?

;; need a push and a pop
;; need functions to implement the operators
;;
;; Retrieve an item from the program and either:
;;   push it onto the stack
;;   execute the operation against the stack
;;     which imples (recursively!) popping items from the stack,
;;                                 operating on them
;;                                 pushing the result back onto the stack...
;;
;;

;; <RPNP> ::= ( <A> . <RPNP> ) | ( <OPRTR> . <RPNP> )
;; "Runs" the RPN program given in list p, stack s
(define run-rpn
  (lambda (p s)
    (if (not (null? p))
        (let ( (next-item (car p)) )
          (if (aop? next-item)
              (let* ((rand2 (car s))
                     (rand1 (cadr s))
                     (new-stack (cddr s))
                     (result (compile (list next-item rand1 rand2)))
                     (rest-of-program (cdr p))
                     )
                (run-rpn rest-of-program (cons result new-stack))
                )
              (run-rpn (cdr p) (cons next-item s)) ) )
        s
        ) ) )
           
;; thin wrapper around rpn-eval
;; which provides it with an empty "runtime stack"
(define rpn-eval
  (lambda (rpnp)
    (run-rpn rpnp '()) ) )
