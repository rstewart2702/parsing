;;
;; Dirt-simple attempt at a grammar for arithmetic expressions which will
;; hopefully be amenable to recursive-descent parsing:

;; <S> is supposed to stand for "Sum or difference"
;; <P> is supposed to stand for "Product or quotient"
;; and the idea is that sums are supposed to have lower precedence,
;; be pushed "lower" in the inorder-traversal of the parse tree,
;; than products, unless that precedence is overridden when
;; a sum is enclosed in parentheses.

;; <S> ::=  <P> + <S>
;;        | <P> - <S>
;;        | <P>

;; <P> ::=  <A> * ( <S> ) | ( <S> ) * <P>
;;        | <A> / ( <S> ) | ( <S> ) / <P>
;;        | <A> *   <P>   
;;        | <A> /   <P>
;;        | <A>

;; We read in the symbols, one at a time, and look ahead to the next
;; symbol, at most, to try and figure out which parsing procedure to
;; invoke next.

;; The idea seems to be to design the grammar in such a way that the next
;; symbol allows the parser to unambiguously determine which parsing
;; procedure to invoke next.

(define atom?
  (lambda (x)
    (or (number? x) (symbol? x))) )

(define prodop?
  (lambda (x)
    (and (symbol? x) (or (eq? x '*) (eq? x '/)))))

(define sumop?
  (lambda (x)
    (and (symbol? x) (or (eq? x '+) (eq? x '-)))))

(define lparen?
  (lambda (x)
    (let ((lp '#\())
      (and (char? x) (eq? x lp)) ) ) )

(define rparen?
  (lambda (x)
    (let ((rp '#\)))
      (and (char? x) (eq? x rp)) ) ) )

;; (define lparen?
;;   (lambda (x)
;;     (let ((lparen '#{(}#))
;;       (and (symbol? x) (eq? x lparen)) )
;;   )
;; )

;; (define rparen?
;;   (lambda (x)
;;     (let ((rparen '#{)}#) )
;;       (and (symbol? x) (eq? x rparen)) )
;;   )
;; )


(define parse-sum
  (lambda (ast p)
    (let ((first  (car p)                            )
          (second (if (null? (cdr p)) '() (cadr p) ) )
          (third  (if (null? (cdr p)) '() (caddr p)) )
          ;; (fourth (if (null? (cadddr p)) '() (cadddr p) ) )
          ;; (remain (cddddr p) )
          )
      (cond
       ;; <P> + <S> | <P> - <S>
       ( (and (sumop? second) (not (null? third)))
         (cons second
               (cons (parse-product first)
                     (cons (parse-sum third remain) '()) ) ) )
       ;; <P>
       ( #t
         (parse-product ast fourth) ) ) ) ) )

(define parse-product
  (lambda (p)
    (let ((first  (car p)                            )
          (second (if (null? (cdr p)) '() (cadr p) ) )
          (third  (if (null? (cdr p)) '() (caddr p)) ) )
      (cond
       ( ;; <A> * <P> | <A> / <P>
        (and (atom? first)
              (prodop? second)
              (not (null? third)) (not (lparen? third)) )
        (cons second
              (cons first
                    (cons (parse-product third) '() ) ) )
        )
       ( ;; <A> * ( <S> ) | <A> / ( <S> )
        (and (atom? first)
             (prodop? second)
             (lparen? third) )
        (cons second
              (cons second
                    (cons (parse-sum third) '()) ) )
        )
       ( ;; ( <S> ) * <A> | ( <S> ) / <A>
        (and (lparen 
        )
       ( ;; <A>
        (and (atom? first) (null? second) (null? third))
        (cons first '()) )
