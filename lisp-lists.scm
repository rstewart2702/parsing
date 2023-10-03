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
      
