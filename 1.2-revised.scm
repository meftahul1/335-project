;; 1.2 Give an inductive definition of the fragment of Scheme implemented by TLS.  Using this
;;     definition, write a purely functional R5RS syntax checker for TLS.  While your syntax checker should not evaluate
;;     its input, it should be as complete as you can make it.  It should, at a minimum, (i) detect
;;     basic errors such as malformed cond and lambda expressions; (ii) detect when primitives are
;;     applied to the wrong number of arguments; and (iii) detect the presence of unbound variables.

;; ————————————————————————————————————————————————————————
;; Inductive definition of the TLS fragment (BNF grammar)
;; ————————————————————————————————————————————————————————

;; <expr>        ::= <number>
;;               |   <symbol>
;;               |   <lambda-expr>
;;               |   <prim-expr>
;;               |   <if-expr>
;;               |   <cond-expr>
;;               |   <app-expr>

;; <expr-seq>    ::= <expr> | <expr> <expr-seq>

;; <prim-expr> ::= "(" <primitive> <expr-seq> ")"
;;
;; <primitive>  ::= "add1" | "sub1" | "zero?" | "null?"
;;               | "car"   | "cdr"
;;               | "+"     | "-"     | "*"     | "/"
;;               | "="     | "<"     | ">"     | "<="    | ">="
;;               | "cons"  | "eq?"

;; <if-expr>     ::= "(" "if" <expr> <expr> <expr> ")"

;; <cond-expr>   ::= "(" "cond" <clause-seq> ")"
;; <clause-seq>  ::= <clause> | <clause> <clause-seq>
;; <clause>      ::= "(" <expr> <expr> ")"
;;               |  "(" "else" <expr> ")"

;; <lambda-expr> ::= "(" "lambda" "(" <symbol-list> ")" <expr> ")"
;; <symbol-list> ::= ε | <symbol> <symbol-list>

;; <app-expr> ::= "(" <expr> <expr-seq> ")"

;; =============================================================================

;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
;; Helper: Decides whether sym is one of our built-in primitives.
;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

(define primitive-list '(add1 sub1 zero? null? car cdr + - * / = < > <= >= cons eq?))

(define list-contains?
  (lambda (t lst)
    (cond
      ((null? lst) #f)
      ((eq? t (car lst)) #t)
      (else
       (list-contains? t (cdr lst))))))
       
(define primitive?
  (lambda (sym)
    (list-contains? sym primitive-list)))

;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
;; Helper: Return the required argument count for primitive p, or signal an error if unknown.
;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

(define lookup-arity
  (lambda (p)
    (cond
      ((eq? p 'add1)    1)
      ((eq? p 'sub1)    1)
      ((eq? p 'zero?)   1)
      ((eq? p 'null?)   1)
      ((eq? p 'car)     1)
      ((eq? p 'cdr)     1)
      ((eq? p '+)       2)
      ((eq? p '-)       2)
      ((eq? p '*)       2)
      ((eq? p '/)       2)
      ((eq? p '>)       2)
      ((eq? p '<)       2)
      ((eq? p '>=)      2)
      ((eq? p '<=)      2)
      ((eq? p '=)       2)
      ((eq? p 'cons)    2)
      ((eq? p 'eq?)     2)
      (else "error: unknown primitive"))))

;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
;; Extend environment by cons’ing var onto env
;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

;; param is a list. 
(define extend-env
  (lambda (param env)
    (append param env)))

;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
;; Function to check constant expression:
;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

(define check-const-expr
  (lambda (expr env)
    (cond
      ((number? expr) 'ok)
      (else
       (begin
         (display "check-syntax expected const-exp, got ")
         (write expr)
         (newline))))))
     
;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
;; Function to check variable expression:
;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

(define bound?
  (lambda (var env)
    (cond
      ((null? env) #f)
      ((eq? var (car env)) #t)
      (else (bound? var (cdr env))))))


(define check-var-expr
  (lambda (expr env)
    (cond
      ((and (symbol? expr) (bound? expr env)) 'ok)
      (else
       (begin
         (display "check-syntax: unbound variable ")
         (write expr)
         (newline))))))
       
;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
;; Function to check lambda expression:
;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

(define all-symbols?
  (lambda (los)
    (cond
      ((null? los) #t)
      ((symbol? (car los)) (all-symbols? (cdr los)))
      (else
       #f))))


(define check-lambda-expr
  (lambda (expr env)
    (cond
      ;; malformed lambda shape
      ((not (= (length expr) 3))
       (begin
         (display "check-syntax: malformed lambda expression")
         (newline)
         'error))

      ;; params must be a proper list
      ((not (list? (cadr expr)))
       (begin
         (display "check-syntax: lambda parameter must be in a list")
         (newline)
         'error))

      ;; all elements in params must be symbols
      ((not (all-symbols? (cadr expr)))
       (begin
         (display "check-syntax: all lambda parameters must be symbols")
         (newline)
         'error))

      ;; extend environment with params and recurse on body
      (else
       (let ((params (cadr expr))
             (body (caddr expr)))
         (check-syntax body (extend-env params env))
         'ok ;; ← syntax correct message
         )))))
       
;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
;; Function to check primitive expression:
;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

(define check-primitive-expr
  (lambda (expr env)
    (let* ((prim (car expr))
           (args (cdr expr))
           (arity (lookup-arity prim)))
      (cond
        ((not (= (length args) arity))
         (begin
           (display "check-syntax: mismatch arity for ")
           (write prim)
           (newline)))

        (else
         (for-each
          (lambda (sub-expr) (check-syntax sub-expr env))
          args)
         'ok)))))

;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
;; Function to check if expression:
;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

(define check-if-expr
  (lambda (expr env)
    (let ((clauses (cdr expr)))
      (cond
        ((not (= (length clauses) 3))
         (begin
           (display "check-syntax: wrong amount of args to if expression")
           (newline)))
        (else
         (for-each
          (lambda (clause) (check-syntax clause env))
          clauses)
         'ok)
        ))))

;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
;; Function to check cond expression:
;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

;; Need to implement this!


;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
;; Function to check application expression:
;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

(define check-app-expr
  (lambda (expr env)
    (cond
      ;; need at least an operator and one operand
      ((null? (cdr expr))
       (begin
         (display "check-syntax: application requires an operator and an operand")
         (newline)))
      (else
       (let ((rator (car expr))
             (rand (cadr expr)))
         (check-syntax rator env)
         (check-syntax rand env))
       'ok))))

;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
;; Main check syntax function:
;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

(define (check-syntax expr env)
  (cond
    ;; 1) Lambda expressions
    ((and (pair? expr) (eq? (car expr) 'lambda))
     (check-lambda-expr expr env))

    ;; 2) Primitive applications
    ((and (pair? expr) (primitive? (car expr)))
     (check-primitive-expr expr env))

    ;; 3) If expressions
    ((and (pair? expr) (eq? (car expr) 'if))
     (check-if-expr expr env))

    ;; 4) Application expressions (anything else that’s a non-empty list)
    ((pair? expr)
     (check-app-expr expr env))

    ;; 5) Numeric constants
    ((number? expr)
     (check-const-expr expr env))

    ;; 6) Variable expression
    ((symbol? expr)
     (check-var-expr expr env))

    ;; 7) Anything else is a syntax error
    (else
     (begin
       (display "check-syntax: unexpected form ")
       (write expr)
       (newline)
       'error))))


;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
;; Setup a dummy environment for testing
;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

(define dummy-env '(x y f g))

;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
;; 1) Constant expression
;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
(define const-expr 42)
(check-syntax const-expr dummy-env)
;; ⇒ 'ok

;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
;; 2) Bound variable
;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
(define var-bound 'x)
(check-syntax var-bound dummy-env)
;; ⇒ 'ok

;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
;; 3) Unbound variable
;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
(define var-unbound 'z)
(check-syntax var-unbound dummy-env)
;; prints: check-syntax: unbound variable z
;; ⇒ 'error

;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
;; 4) Primitive – correct arity
;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
(define prim-correct '(add1 x))
(check-syntax prim-correct dummy-env)
;; ⇒ 'ok

;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
;; 5) Primitive – wrong arity
;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
(define prim-wrong '(cons 1 2 3))
(check-syntax prim-wrong dummy-env)
;; prints: check-syntax: mismatch arity for cons
;; ⇒ 'error

;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
;; 6) If – well-formed
;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
(define if-correct '(if (> x y) x y))
(check-syntax if-correct dummy-env)
;; ⇒ 'ok

;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
;; 7) If – wrong number of clauses
;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
(define if-wrong '(if x y))
(check-syntax if-wrong dummy-env)
;; prints: check-syntax: wrong amount of args to if expression
;; ⇒ 'error

;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
;; 8) Application – well-formed
;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
(define dummy-env2 '(f x))
(define app-correct '(f x))
(check-syntax app-correct dummy-env2)
;; ⇒ 'ok

;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
;; 9) Application – missing operand
;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
(define app-bad '(f))
(check-syntax app-bad dummy-env2)
;; prints: check-syntax: application requires an operator and an operand
;; ⇒ 'error

;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
;; 10) Nested expression
;;––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
(define nested '(lambda (x) (if (zero? x) (sub1 x) (add1 x))))
(check-syntax nested '(x))
;; ⇒ 'ok


(define nested-lambda '(lambda (x)
                         (lambda (y)
                           (lambda (z) (+ x y))))) 
                          

  

