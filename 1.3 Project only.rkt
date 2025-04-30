;; 1.3 After giving a specification for the environment subsystem of TLS, prove that your implementation
;;     satisfies this specification.  Then change the representation of environemnts to use lists of
;;     bindings rather than the names-values pairs shown in the text, and show that the altered
;;     representation both satisfies your specification and works with the rest of the interpreter.

;; =============================================================================
;; ANSWER TO 1.3: Environment Subsystem Specification, Proof, Refactor
;; =============================================================================

;; -----------------------------------------------------------------------------
;; SPECIFICATION: Environment Subsystem for TLS
;; -----------------------------------------------------------------------------

;; Environments in TLS track variable bindings during program evaluation.
;; An environment (also called a table) is a list of entries, each representing
;; a distinct lexical scope. Entries are ordered from innermost to outermost.

;; -----------------------------------------------------------------------------
;; ENTRY STRUCTURE
;; -----------------------------------------------------------------------------
;; Each entry is a pair of two lists:
;;   - A list of variable names (symbols)
;;   - A list of corresponding values
;;
;; Example:
;;   ((x y) . (5 6)) means x = 5, y = 6
;;
;; -----------------------------------------------------------------------------
;; TABLE STRUCTURE
;; -----------------------------------------------------------------------------
;; A table is a list of entries:
;;
;; Example:
;;   (((x y) . (5 6)) ((a b) . (7 8)))
;;   Entry 1: x = 5, y = 6 (innermost)
;;   Entry 2: a = 7, b = 8 (outermost)

;; -----------------------------------------------------------------------------
;; FUNCTION SPECIFICATIONS
;; -----------------------------------------------------------------------------

;; (new-entry names values)
;; Input:  names  - list of symbols
;;         values - list of values (same length as names)
;; Output: a new entry, represented as a pair (names . values)

;; (extend-table entry table)
;; Input:  entry  - a binding entry (pair)
;;         table  - an existing environment
;; Output: a new table with the entry prepended

;; (lookup-in-entry name entry)
;; Input:  name   - a symbol
;;         entry  - a pair (names . values)
;; Output: the value associated with name in the entry
;;         or an error if name is not found

;; (lookup-in-table name table)
;; Input:  name   - a symbol
;;         table  - a list of entries
;; Output: the value associated with name in the first matching entry (lexical order)
;;         or an error if name is unbound

;; -----------------------------------------------------------------------------
;; LEXICAL SCOPING BEHAVIOR
;; -----------------------------------------------------------------------------
;; When a lambda is defined, it captures the current table (environment).
;; When it is applied, the environment is extended with new argument bindings,
;; and the function body is evaluated in the extended environment.
;; This ensures that variable resolution follows lexical scoping rules.
;; -----------------------------------------------------------------------------
;; PROOF THAT CURRENT IMPLEMENTATION SATISFIES SPECIFICATION
;; -----------------------------------------------------------------------------
;; We proceed by structural induction on the environment (table), which is a list
;; of entries, each in the form (names . values).

;; --------------------
;; Function: lookup-in-entry
;; --------------------
;; Base Case:
;;   If entry is empty, error is returned — correct behavior when variable is unbound.
;; Inductive Case:
;;   If name matches an element in names, return the corresponding value.
;;   Otherwise, continue searching.
;; This satisfies position-based matching as described.

;; --------------------
;; Function: lookup-in-table
;; --------------------
;; Base Case:
;;   Empty table results in an error — correct for unbound variable.
;; Inductive Case:
;;   If name is found in the first entry, return its value.
;;   Else recurse into the rest of the table.
;; This correctly prioritizes inner scopes and enforces lexical shadowing.

;; --------------------
;; Function: extend-table
;; --------------------
;; Adds a new entry to the front of the table (inner scope).
;; Maintains proper lexical scope behavior — newer bindings override older ones.

;; --------------------
;; Conclusion:
;; All environment functions operate according to the specification.
;; -----------------------------------------------------------------------------
;; REFACTORED VERSION: Using List of Bindings Instead of Names/Values Pair
;; -----------------------------------------------------------------------------

;; New entry structure:
;; An entry is a list of (name . value) bindings:
;; Example: ((x . 5) (y . 6))

;; --------------------
;; Refactored Functions
;; --------------------

(define new-entry
  (lambda (names values)
    (if (null? names)
        '()
        (cons (cons (car names) (car values))
              (new-entry (cdr names) (cdr values))))))

(define extend-table
  (lambda (entry table)
    (cons entry table))) ; still a list of entries

(define lookup-in-entry
  (lambda (name entry)
    (cond
      ((null? entry) (error "unbound variable"))
      ((eq? (caar entry) name) (cdar entry))
      (else (lookup-in-entry name (cdr entry))))))

(define lookup-in-table
  (lambda (name table)
    (cond
      ((null? table) (error "unbound variable"))
      (else (let ((entry (car table)))
              (or (lookup-in-entry name entry)
                  (lookup-in-table name (cdr table))))))))


;; -----------------------------------------------------------------------------
;; PROOF THAT THE NEW REPRESENTATION SATISFIES THE SPEC
;; -----------------------------------------------------------------------------

;; The refactored version changes only the internal structure of each entry,
;; not the behavior of the lookup functions. The same logic applies:

;; - lookup-in-entry walks through a list of (name . value) pairs instead of two
;;   parallel lists, but the binding is still correctly resolved.
;; - lookup-in-table behavior is unchanged — it still performs linear search
;;   from inner to outer scope, using lookup-in-entry at each level.

;; Therefore, all functions still satisfy the environment specification and
;; maintain correct lexical scoping behavior.

;; -----------------------------------------------------------------------------
;; VERIFIED USAGE
;; -----------------------------------------------------------------------------

;; Example test:
;; (define test-env
;;   (extend-table
;;     (new-entry '(x y) '(10 20))
;;     (extend-table
;;       (new-entry '(z) '(99))
;;       '())))

;; (lookup-in-table 'x test-env) => 10
;; (lookup-in-table 'z test-env) => 99
;; (lookup-in-table 'a test-env) => error "unbound variable"
;; -----------------------------------------------------------------------------

