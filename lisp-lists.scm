(define atom?
  (lambda (x)
    (or (number? x) (symbol? x))) )

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
;; ====================================

;; "general lisp lists"
;; <GT> ::= <A> | <L>
(define general-term?
  (lambda (t)
    (or
     (atom? t)
     (general-list? t))))

;; <L> ::= '() | <GT> . <L>
(define general-list?
  (lambda (t)
    (or
     (null? t)
     (and
      (general-term? (car t))
      (general-list? (cdr t)) ) ) ) )


;; ====================================

;; "flat" lists, list-of-atoms:
;; <FT> ::= <A> | <LA>
(define flat-term?
  (lambda (t)
    (or
     (atom? t)
     (list-of-atoms? t))) )

;; <LA> ::= '() | <A> . <LA>
(define list-of-atoms?
  (lambda (t)
    (or
     (null? t)
     (and
      (atom? (car t))
      (list-of-atoms? (cdr t))) ) ) )

  
;; ====================================

;; Recognizing something as belonging to a language
;; is one thing.
;;
;; Parsing the language to produce a parse tree is
;; something else.

;; An attempt at a "flat sums" kind of language/grammar:
;; FS ::= <A> | <SS>

(define flat-sum-parse
  (lambda (s)
    (if (atom? s) (cons s '())
        (simple-sum-parse s)
        )
    )
  )

;; N.B. trying to use Lisp-style list structure here,
;; the parentheses are NOT symbols in the input language.
;; <SS> ::= '() | <FS> . ( <+-> . <SS> ) 
(define simple-sum-parse
  (lambda (s)
    (if (null? s) '()
        (let* ((first (car s))
               (second
                (if (null? (cdr s)) '() ;; this is an ERROR
                    (if (sumop? (cadr s)) (cadr s)
                        '() ;; this is an ERROR
                        )))
               (remaining (cddr s))
               (fr (flat-sum-parse first))
               (ptail (simple-sum-parse remaining)) )
          (cons
           second
           (cons fr ptail))) ) ) )
      
;; Rethinking the grammar, starting with a "simple-sum."
;; Herein, the notion is that we're using Lisp lists,
;; and so the idea is that a Lisp list is a cons cell,
;; and therefore 

;; I *think* this might work properly:
;; The idea is that a sum is either <A> or <A> followed by <RS>.
;; <RS> is a plus-or-minus, followed by a list, i.e.,
;;   <A> . <RL> 
;; Then, the <RL> is either empty-list, so that it's possible for
;;   <A> . <RL> == (<A>)
;; or else <RL> is a list with plus-or-minus as head, 
;; and tail made up of:
;;   <A> . <RL>
;; which allows recursion to continue.

;; <SS> ::=  <A>
;;         | <A> . <RS> 

;; <RS> ::= <SUMOP> . ( <A> . <RL> )
;; OR, taking tips from the Slonneger text, but results in the third
;;     <RL> production which follows:
;; <RS> ::=  '()
;;         | <SUMOP> . ( <A> . <RS> )

;; <RL> ::=  '()
;;         | <SUMOP> . ( <A> . <RL> )
;;
;; (But again, the above seemed rather complicated, and I tried to
;; simplify it further, below.)

;; SHOULDN'T THIS BE SIMPLIFIED FURTHER?
;; THE FOREGOING HAS MANY REPEATED STRUCTURES,
;; AND SIMPLIFYING IT WOULD SIMPLIFY THE PARSING
;; PROCEDURES, WOULDN'T IT?

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

(define simple-sum-parse
  (lambda (s)
    (if (atom? s) s
        (rest-of-sum-parse s) ;; assumption is that it is a list.
        ) ) )

;; This seems to get us part of the way there,
;; but it does not check (sumop? rator)
(define rest-of-sum-parse
  (lambda (s)
    (let* ((rand (car s))
           (rator (cadr s))
           (rand2 (cddr s))
           (rs-parse (rest-of-sum-list-parse rand2) ) )
      (cons rator
            (cons rand rs-parse) ) ) ) )


(define rest-of-sum-list-parse
  (lambda (s)
    (cond
     ((null? (cdr s)) s)
     (#t (let* ((rand (car s))
                (rator (cadr s))
                (rand2 (cddr s))
                (parsed-rest (rest-of-sum-list-parse rand2)) )
           (list
            (cons rator
                  (cons rand parsed-rest) ) ) ) ) ) ) )
         
;; Now, the above three parsing procedures make a very simple little parse
;; for addition-and-subtraction expressions, which derives a "parse tree"
;; which can be used to derive Forth expressions via a postorder traversal.
;;
;; The next tricky bit will be to try and work division and multiplication
;; into it, giving precedence to division and multiplication, and allow
;; precedence to be overridden with the use of enclosing parentheses around
;; "addition expressions."

;; Considering multiplication now:
;;
;; a + b * c == a + (b*c) , i.e., multiplication takes higher precedence.
;; (+ a (* b c)) should be the parse tree.
;;
;; a * b + c should yield (+ (* a b) c)
;;
;; a * (b + c) should yield (* a (+ b c))
;;
;; (b + c) * a should yield (* (+ b c) a)
;;
;; So it would seem that "bracketing" should cause recursive parsing
;; of <SS>.  But how do the two interact?  For a sum can have products
;; as its terms, and a product can have sums as its factors.
;; In the following, I've mimiced in product what was done in sums,
;; with the addition of <T> for "terms in a sum" and
;; <F> for "factors in a product."
;;
;; <SS> ::=  <T>
;;         | <RS>
;; <T>  ::=  <F>
;;         | <A>
;; <RS> ::=  <T>
;;         | ( <SUMOP> . <RL> )
;; <RL> ::=  <T> . '()
;;         | <T> . ( <SUMOP> . <RL> )
;;
;; <F>  ::=  <LB> <SS> <RB>
;;         | <A>
;;
;; <P>  ::=  <F>
;;         | <RP>
;; <RP> ::=  <F>
;;         | ( <MULOP> . <RPL> )
;; <RPL>::=  <F> . '()
;;         | <F> . ( <MULOP> . <RPL> )

;; I endeavored to rewrite the above, since
;; some of the production rules seemed to be redundant
;; and possibly define a language different from the
;; language I wish to recognize.
;;
;; Also, in the interests of simplicity-for-study,
;; the following likely only permits sums which are
;; ALWAYS bracketed (in parentheses, by default, eh?)
;; Therefore, the starting production/state will
;; be to try and recognize/parse a <P>, a "product,"
;; first, and then allow solitary sums that are
;; start out bracketed in parentheses.

;; <P>  ::=   <F>
;;          | <RP>
;; <RP> ::=   <F> . ( <MULOP> . <RPL> )
;; <RPL> ::=   <F> . '()
;;           | <F> . ( <MULOP> . <RPL> )
;;
;; <F> ::=  <A>
;;        | <LB> <SS> <RB>
;;
;; <SS> ::=  <T>
;;         | <RS>
;; <T>  ::=  <P>
;;         | <A>
;; <RS> ::=  <T> . ( <SUMOP> . <RL> )
;; <RL> ::=  <T> . '()
;;         | <T> . ( <SUMOP> . <RL> )

;; Now, how on earth are products to be given
;; higher precedence when there are not
;; parentheses?

;; E.g., how would the above parse/recognize
;; something like
;;   b * a + c
;; For this, we would like a parse tree like:
;;   (+ (* b a) c)
;;
;; And then, how would the above parse/recognize
;; something like
;;   (b + a * c)
;;      notice:  the outermost expr must be
;;               ()-bracketed, correct?
;; because we would like to see a parse tree like:
;;   (+ b (* a c) )
;; I suppose...

;; When parsing (b + a * c):
;; (b   +         a    *     c)
;; ^^   ^         ^
;; ||   |         |
;; \<A> \         |
;;  \    \        
;;   <LB> <SUMOP>


;;============================================
;; So, here's a third revision to the language:
;;
;; <SS> IS THE START SYMBOL.
;; <SS> ::=  <P> | (<P> . <RSL>)
;; <RSL> ::= ( <SUMOP> . <RSL1> )
;; <RSL1> ::=  ( <P> ) | ( <P> . ( <SUMOP> . <RSL1> ) )
;;

(define ss-sum
  (lambda (s)
    (cond ((null? s) '())
          ((null? (cdr s)) (product (car s)) )
          (#t (let* ((rand (product (car s)))
                     (rsl-parse (rsl (cdr s)))
                     (rator (car rsl-parse))
                     (rand2 (cdr rsl-parse)) )
                (display "rsl-parse: ")(display rsl-parse)(display "\n")
                (display "rand2: ")(display rand2)(display "\n")
                (cons rator
                      (cons rand rand2)) ) ) ) ) )

;; Starting on a revision to the ss-sum in which the result is a
;; parsed item, and the resulting sub-parse-tree?
(define ss-sum-r
  (lambda (s)
    (cond ((null? s) '())
          ((null? (cdr s))
           (product (car s))  ;; but this is TOO SIMPLE, there's more to dealing with result of
                              ;; parsing a <P> (This probably needs to be another "let*" expression that
                              ;; derives the parse-result and the rest-of-tokens-list to return to caller...)
           )
          )
          (#t
           (let* ((prod-parse (product (car s)))  ;; prod-parse is "result-of-parsing-first-product, rest-of-tokens" pairing, eh?
                                                  ;; prod-parse is bound to ( <P> <TOKENS> ) list?
                  (rand       (car prod-parse)      )   ;;
                  (rsl-parse  (rsl (cdr prod-parse)))   ;; and so (cdr prod-parse) is the list-of-remaining-tokens?
                                                        ;; rsl-parse is bound to ( <RSL> <TOKENS> )
                  (rator      (caar rsl-parse)      )   ;; and so (caar rsl-parse) is the operator retrieved from rsl-parse result
                  (rand2      (cadr rsl-parse)      ) ) ;; and so (cadr rsl-parse) is second operand retrieved from rsl-parse result
             (cons
              (cons rator
                    (cons rand rand2) )
              (cdr prod-parse) ;; bring in the rest-of-tokens, eh?
              )
           )
          ) ) )


(define rsl
  (lambda (s)
    (let* ((rator (if (sumop? (car s)) (car s) '()))
           (rand  (rsl1 (cdr s))) )
      (cons rator rand)) ) )

(define rsl1
  (lambda (s)
    (cond ((null? (cdr s)) (cons (product (car s)) '()) )
          (#t (let* ((rator (if (sumop? (cadr s)) (cadr s) '()))
                     (rand  (product (car s)))
                     (rand2 (rsl1 (cddr s))  ) )
                (list
                 (cons rator
                       (cons rand rand2) ) ) ) ) ) ) )

;; <P> ::=  <F> | ( <F> . <RPL> )
;; <RPL> ::= ( <MULOP> . <RPL1> )
;; <RPL1> ::= ( <F> ) | ( <F> . ( <MULOP> . <RPL1> ) )
;;
;; <F> ::=  <A> | <LB> <SS> <RB>

;; for now, this is all parsing a product will do:
(define product
  (lambda (s)
    (display "product \n") (display s) (display "\n")
    (cond ((null? s) '())
          ((null? (cdr s)) (cons (factor (car s)) '()))
          (#t (let* ((fctr (factor (car s)))
                     (rpl-parse (rpl (cdr s)))
                     (rator (car rpl-parse))
                     (fctr2 (cdr rpl-parse)) )
                (cons rator
                      (cons fctr fctr2)) ) ) ) ) )

(define rpl
  (lambda (s)
    (let* ((rator (if (mulop? (car s)) (car s) '()))
           (fctr (rpl1 (cdr s))) )
      (cons rator fctr) )
    ) )

(define rpl1
  (lambda (s)
    (cond ((null? (cdr s)) (cons (factor (car s)) '()) )
          (#t (let* ((rator (if (mulop? (cadr s)) (cadr s) '()))
                     (fctr  (factor (car s) ))
                     (fctr2 (rpl1   (cddr s))) )
                (list
                 (cons rator
                       (cons fctr fctr2) ) ) ) ) )
    ) )

;; ATTENTION:
;; One thing we seem to have forgotten is that the
;; left- and right-bracket delimiters (parentheses in "ordinary notation")
;; need to be consumed, right?  They'll need to be used up before
;; returning the result to the enclosing or calling parsing function.
;; This means that the grammar needs to be revised.
;;
;; That is, the rule:
;;   <F> ::= <A> | <LB> <SS> <RB>
;; needs to be broken into at least two separate rules?
;;
;;   <F> ::= <A> | <LB> <FR>
;;   <FR> ::= <SS> <RB> 
;;
;; It's almost as if we must scan ahead to the enclosing <RB>
;; and pass everything in between <LB> and <RB> into a
;; "separate invocation of the parser," and return that result.
;; So, this seems to imply a "plucking out" of all that's
;; delimited between <LB> and <RB> into a separate list-of-tokens,
;; parsing it "separately," and then returning that result
;; to the "enclosing caller" so that the "main parsing" may
;; continue.  We're invoking a sub-parser with the results of
;; what's been consumed from between <LB> and <RB>.


(define factor
  (lambda (f)
    (cond ((null? (cdr f)) (car f))
          ((lbrack? (car f)) (list (ss-sum (cadr f)) ) ) ) ) )

;; Need a mechanism to recursively parse the <SS> element,
;; then return "the rest of the tokens list" to the caller
;; after the recursive work is concluded...
;;
;; Another rule-and-parsing-procedure seems to be called for.
;; The parsing procedure will recursively invoke ss-sum, 
;; and return the result of ss-sum, AND the rest-of-the-tokens-list.
;; The new parsing procedure provides a "stacked context" in which
;; this can be "captured," and then it can "tail-recursively" continue
;; with the parsing.  That is to say, this "handler of the bracketed <SS>"
;; should try to parse the <SS> and also return to its caller
;; the rest of the tokens-to-be-parsed, which ought to start with
;; the <RB>, that "right-hand bracket" which delimited the <SS>-that-was-parsed-out...

(define b-factor-finish
  (lambda (s)
    ) )
    

;; I"m afraid that I must thread through all of the parsing-procedures
;; the list-of-tokens, and return a list-of-remaining-tokens
;; from each parsing procedure call?
