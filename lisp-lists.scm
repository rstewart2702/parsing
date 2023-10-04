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

;; <RL> ::=  '()
;;         | <SUMOP> . ( <A> . <RL> )

;; SHOULDN'T THIS BE SIMPLIFIED FURTHER?
;; THE FOREGOING HAS MANY REPEATED STRUCTURES,
;; AND SIMPLIFYING IT WOULD SIMPLIFY THE PARSING
;; PROCEDURES, WOULDN'T IT?

;; <SS> ::=  <A> | <RS>
;;
;; <RS> ::=  <A> . ( <SUMOP> . <RL> )
;;
;; <RL> ::=  '()
;;         | <A> . ( <SUMOP> . <RL> )

(define simple-sum-parse
  (lambda (s)
    (if (and (list? s) (atom? (car s)))
	(rest-of-sum-parse (cdr s))
	s)))

(define rest-of-sum-parse
  (lambda (s)
    (let* ((rand (car s))
	   (rator (cadr s))
	   (rand2 (caddr s)))
      (cons rator
	    (cons rand
		  (rest-of-sum-list-parse rand2)))) ) )

(define rest-of-sum-list-parse
  (lambda (s)
    (if (null? s) s
	(let* ((rand (car s))
	       (rator (cadr s))
	       (rand2 (caddr s)))
	  (cons rator
		(cons rand
		      (rest-of-sum-list-parse rand2)))))))
	 


;; FOR SOME REASON, THIS WON'T WORK?
;; <SS> ::=  '()
;;         | <A> . <RS>
;;
;; <RS> ::=  <SUMOP> . <SS>
;;         | '()


