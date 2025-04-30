
;; Check if x is atom
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; Add 1 to n
(define add1
  (lambda (n)
    (+ n 1)))

;; Subtract 1 from n
(define sub1
  (lambda (n)
    (- n 1)))

;; Check if atoms are equal
(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2))
      (= a1 a2))
     ((or (number? a1) (number? a2))
      #f)
     (else 
      (eq? a1 a2)))))

;; Check if lists are equal
(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else
      (and (equal? (car l1) (car l2))
	   (eqlist? (cdr l1) (cdr l2)))))))

;; Check if two S-expressions are equal
(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2))
      (eqan? s1 s2))
     ((or (atom? s1) (atom? s2))
      #f)
     (else
      (eqlist? s1 s2)))))

;; Get first item
(define first car)

;; Get second item
(define second cadr)

;; Get third item 
(define third caddr)

;; Build pair 
(define build
  (lambda (s1 s2)
    (cons s1
	  (cons s2 '()))))

;; Build entry from a set of names and a list of values
(define new-entry build)

;; Look up name in entry
;; Call entry-f if not found.
(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
			  (first entry)  ; names
			  (second entry) ; values
			  entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
     ((null? names) (entry-f name))
     ((eq? (car names) name)
      (car values))
     (else
      (lookup-in-entry-help name
			    (cdr names)
			    (cdr values)
			    entry-f)))))

;; Table(environment) is list of entries
(define extend-table cons)

;; Look for name in table
;; Call table-f if name is not found
(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table) (table-f name))
     (else
      (lookup-in-entry name
		       (car table)    ;current entry
		       (lambda (name) ;if not found in current entry, look in next entry
			 (lookup-in-table name
					  (cdr table)
					  table-f)))))))

;; Equivalent of eval
(define value
  (lambda (e)
    (meaning e '()))) ;'() is empty table

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

;; Produce correct function for each possible S-expression
(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e) (atom-to-action e))
     (else (list-to-action e)))))

;; Call *const function if e is number, boolean, or primitive operators
;; Else it is a variable
(define atom-to-action
  (lambda (e)
    (cond
     ((number? e) *const)
     ((eq? e #t)  *const)
     ((eq? e #f)  *const)
     ((eq? e 'cons) *const)
     ((eq? e 'car)  *const)
     ((eq? e 'cdr)  *const)
     ((eq? e 'null?) *const)
     ((eq? e 'eq?)   *const)
     ((eq? e 'atom?) *const)
     ((eq? e 'zero?) *const)
     ((eq? e 'add1)  *const)
     ((eq? e 'sub1)  *const)
     ((eq? e 'number?) *const)
     (else *identifier))))

;; Check the car of e to see if it is quote, lambda, or cond
;; Else it is *application
(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e))
      (cond
       ((eq? (car e) 'quote) *quote)
       ((eq? (car e) 'lambda) *lambda)
       ((eq? (car e) 'cond) *cond)
       (else *application)))
     (else *application))))

;; Action for constants
(define *const
  (lambda (e table)
    (cond
     ((number? e) e) ; return itself
     ((eq? e #t) #t) ; return itself
     ((eq? e #f) #f) ; return itself
     (else
      (build 'primitive e))))) ; attach primitive tag on expression

;; Action for quote
(define *quote
  (lambda (e table)
    (second e))) ; Return value following quote

;; Action for variables
(define *identifier
  (lambda (e table)
    (lookup-in-table e table initital-table))) ; Look for symbol in table

;; Called when identifier is not found in environment
(define initital-table
  (lambda (name)
    (car '())))

;; Action for lambda
(define *lambda
  (lambda (e table)
    (build 'non-primitive          ; for lambdas, attach non-primitive tag
	   (cons table (cdr e))))) ; add args and body to table

;; Action for conditionals
(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr) ; List of predicates and actions

(define evcon
  (lambda (lines table)
    (cond
     ((else? (question-of (car lines)))
      (meaning (answer-of (car lines))
	       table))
     ((meaning (question-of (car lines))
	       table)
      (meaning (answer-of (car lines))
	       table))
     (else (evcon (cdr lines) table)))))

;; Check if the predicate is else
(define else? 
  (lambda (x)
    (cond
     ((atom? x)
      (eq? x 'else))
     (else
      #f))))

(define question-of first)
(define answer-of   second)

;; Action for application
;; Apply the meaning of function to meaning of argument to get new expression and environment
(define *application
  (lambda (e table)
    (apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)
(define arguments-of cdr)

(define evlis
  (lambda (args table)
    (cond
     ((null? args) '())
     (else
      (cons (meaning (car args) table)
	    (evlis (cdr args) table))))))

(define apply
  (lambda (fun vals)
    (cond
     ((primitive? fun)
      (apply-primitive (second fun)
		       vals))
     ((non-primitive? fun)
      (apply-closure (second fun)
		     vals)))))

(define primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))

(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive)))

(define apply-primitive
  (lambda (name vals)
    (cond
     ((eq? name 'cons)
      (cons (first vals) (second vals)))
     ((eq? name 'car)
      (car (first vals)))
     ((eq? name 'cdr)
      (cdr (first vals)))
     ((eq? name 'null?)
      (null? (first vals)))
     ((eq? name 'eq?)
      (eq? (first vals) (second vals)))
     ((eq? name 'atom?)
      (:atom? (first vals)))
     ((eq? name 'zero?)
      (zero? (first vals)))
     ((eq? name 'add1)
      (add1 (first vals)))
     ((eq? name 'sub1)
      (sub1 (first vals)))
     ((eq? name 'number?)
      (number? (first vals))))))

(define :atom?
  (lambda (x)
    (cond
     ((atom? x) #t)
     ((null? x) #f)
     ((eq? (car x) 'primitive) #t)
     ((eq? (car x) 'non-primitive) #t)
     (else #f))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
	     (extend-table 
	      (new-entry
	       (formals-of closure)
	       vals)
	      (table-of closure)))))

(define table-of first)    ; get table of *lambda
(define formals-of second) ; get formal arguments of *lambda
(define body-of third)     ; get body of *lambda
  

      
