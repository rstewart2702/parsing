(define atom?
  (lambda (x)
    (or (number? x) (symbol? x))) )

(define sumop?
  (lambda (x)
    (and (symbol? x) (or (eq? x '+) (eq? x '-)))))

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
;; BUT: TURNS OUT THIS *FAILS* TO SPECIFY THE LANGUAGE
;;      WE WANTED!
;; <SS> IS THE START SYMBOL.
;; <SS> ::=  <P> | (<P> . <RSL>)
;; <RSL> ::= ( <SUMOP> . <RSL1> )
;; <RSL1> ::= <P> | ( <SUMOP> . <RSL1> )
;;
;; <P> ::=  <F> | ( <F> . <RPL> )
;; <RPL> ::= ( <MULOP> . <RPL1> )
;; <RPL1> ::= <F> | ( <F> . ( <MULOP> . <RPL1> ) )
;;
;; <F> ::=  <A> | <LB> <SS> <RB>

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

(define rsl
  (lambda (s)
    (let* ((rator (if (sumop? (car s)) (car s) '()))
           (rand  (rsl1 (cdr s))) )
      (cons rator rand) ) ) )

(define rsl1
  (lambda (s)
    (cond ((null? (cdr s)) (product (car s)))
          (#t (let* ((rator (if (sumop? (car s)) (car s) '()))
                     (rand (rsl1 (cdr s))) )
                (cons rator
                      rand) ) ) ) ) )

;; for now, this is all parsing a product will do:
(define product
  (lambda (s)
    (display "product \n") (display s) (display "\n")
    (cons s '())))
                     
