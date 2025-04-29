


;; CSc 335 Project Spring 2025 Part 1
;; Version 1.0
;; Released March 31 2025


;; In Chapter 10 of The Little Schemer, the authors present an interpreter for a part of Scheme.  I will refer
;; to this interpreter and to the language it implements as TLS Scheme, or just TLS. 

;; 1.1 Implement TLS in pure functional R5RS, providing a full development for the code including specifications
;;     for each function.  Give examples of TLS programs which work with your interpreter.

;; 1.2 Give an inductive definition of the fragment of Scheme implemented by TLS.  Using this
;;     definition, write a purely functional R5RS syntax checker for TLS.  While your syntax checker should not evaluate
;;     its input, it should be as complete as you can make it.  It should, at a minimum, (i) detect
;;     basic errors such as malformed cond and lambda expressions; (ii) detect when primitives are
;;     applied to the wrong number of arguments; and (iii) detect the presence of unbound variables.

;; 1.3 After giving a specification for the environment subsystem of TLS, prove that your implementation
;;     satisfies this specification.  Then change the representation of environemnts to use lists of
;;     bindings rather than the names-values pairs shown in the text, and show that the altered
;;     representation both satisfies your specification and works with the rest of the interpreter.

;; 1.4 Research closures and lexical scope, and prove that (your implementation of) TLS implements these
;;     correctly.  Your writeup will need to include enough information on closures and lexical scope
;;     to allow a meaningful correctness discussion.  Your argument will use structural, as well as other,
;;     inductions. 

;; 1.5 After carefully identifying a standard of correctness, prove that your implementation of TLS is
;;     correct according to that standard.

;; 1.6 Carefully explain the dependence of TLS on the underlying R5RS of DrRacket.  Focus, in particular,
;;     on the mechanics of function calling: which system does which work?

;; 1.7 Drawing on Chapter 9 of The Little Schemer, equip your TLS with recursion to form TLS-REC, using the Y-combinator.
;;     Research Y-combinators, and prove that the implementation you use actually implements a Y-combinator.
;;     Explain, in detail, how the Y-combinator implements recursion.  Include interesting examples
;;     of recursive programs written TLS-REC.  


;; Part 2 of the project, extending Part 1, will be released on Wednesday, April 23.   



;; Your writeup should be prepared as an executable .scm file, with text enclosed in boxed comments.

;; The project is to be completed in teams of THREE students, and Part 1 is due in my inbox no later than
;; midnight, Wednesday May 7.  Oral defenses will be conducted (by Zoom) from Thursday May 8 through
;; Wednesday May 14 -- a signup sheet will be posted later.

;; Students in any one team must have midterm averages within 2 letter grades of each other.


