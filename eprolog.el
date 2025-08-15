;;; eprolog.el --- Prolog engine implementation in Emacs Lisp -*- lexical-binding: t -*-

;; Copyright (C) 2025 Masaya Taniguchi

;; Author: Masaya Taniguchi
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: languages, prolog, logic programming
;; URL: https://github.com/tani/eprolog

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ε-prolog (eprolog) is a complete Prolog engine implementation written
;; in pure Emacs Lisp.  It provides a fully functional Prolog system
;; integrated into the Emacs environment, offering traditional Prolog
;; programming capabilities with seamless Lisp interoperability.
;;
;; Features:
;; - Complete unification algorithm with occurs check
;; - Backtracking and choice points with proper cut (!) semantics
;; - Clause database management for facts and rules
;; - Interactive query execution with solution enumeration
;; - Built-in predicates for type checking, control, and list operations
;; - Definite Clause Grammar (DCG) support
;; - Spy/debugging functionality for tracing execution
;; - Direct Lisp integration through special predicates
;;
;; Quick Start:
;;
;;   ;; Define facts and rules
;;   (eprolog-define-prolog-predicate parent (tom bob))
;;   (eprolog-define-prolog-predicate parent (bob ann))
;;
;;   (eprolog-define-prolog-predicate grandparent (_x _z)
;;     (parent _x _y)
;;     (parent _y _z))
;;
;;   ;; Query the database
;;   (eprolog-query (grandparent tom _x))
;;
;; See README.org for detailed documentation.

;;; Code:

(require 'cl-lib)

;;; Core Data Types & Configuration

(cl-defstruct eprolog--failure
  "Represents a failed Prolog computation.
Used throughout the engine to indicate when unification, goal proving,
or other operations cannot succeed.")

(cl-defstruct eprolog--success
  "Represents a successful Prolog computation.
BINDINGS is an alist of variable-value pairs from unification.
CONTINUATION is a function that, when called, attempts to find
the next solution via backtracking."
  bindings
  continuation)

(define-error 'eprolog-cut-exception "Cut exception" 'error)

;; Public configuration variables

(defvar eprolog-clause-database '()
  "The current Prolog clause database.
An alist where keys are predicate symbols and values are lists of clauses.
Each clause is a list where the first element is the head and remaining
elements form the body.")

(defvar eprolog-spy-predicates '()
  "List of predicate symbols to spy on during execution.
When a predicate in this list is called, debugging information
will be displayed according to `eprolog-spy-state'.")

(defvar eprolog-spy-state 'prompt
  "Current spy mode for debugging.
Valid values are:
- \\='prompt: Ask user for each spy action
- \\='always: Show all spy messages without prompting
- \\='disabled: No spy output")

(defvar eprolog-occurs-check t
  "Whether to perform occurs check during unification.
When non-nil, prevents creation of infinite structures by checking
if a variable occurs within the term it's being unified with.")

;; Internal variables

(defvar eprolog-current-bindings '()
  "Current variable bindings during proof.
Dynamic variable used internally by the proof engine to track
variable assignments during goal resolution.")

(defvar eprolog-remaining-goals '()
  "Remaining goals to prove in current proof context.
Dynamic variable used internally by the proof engine to track
the goal stack during resolution.")

(defvar eprolog-dynamic-parameters '()
  "Dynamic parameter storage for Prolog-Lisp interaction.
An alist of `symbol-value' pairs used by the dynamic-put and dynamic-get
predicates to maintain state across Prolog goals.")

;;; Variable & Term Operations

(defun eprolog--variable-p (term)
  "Test if TERM is a Prolog variable.
A Prolog variable is a symbol whose name begins with '_'.
This includes both named variables like _X and the anonymous variable _.

Returns non-nil if TERM is a Prolog variable, nil otherwise."
  (and (symbolp term)
       (let ((symbol-string (symbol-name term)))
         (and (> (length symbol-string) 0)
              (= (aref symbol-string 0) ?_)))))

(defun eprolog--named-variable-p (term)
  "Test if TERM is a named Prolog variable (not anonymous).
Returns non-nil if TERM is a variable but not the anonymous variable '_'.
Used to distinguish between _X (named) and _ (anonymous) variables."
  (and (eprolog--variable-p term)
       (not (and (symbolp term) (eq term '_)))))

(defun eprolog--lookup-variable (variable bindings)
  "Look up VARIABLE's value in the association list BINDINGS.
VARIABLE should be a Prolog variable symbol.
BINDINGS is an alist of (variable . value) pairs.

Returns the value bound to VARIABLE, or nil if not found."
  (cdr (assoc variable bindings)))

(defun eprolog--substitute-bindings (bindings expression &optional visited)
  "Apply variable BINDINGS to EXPRESSION, replacing bound variables.
BINDINGS is an alist of variable-value pairs, or a failure object.
EXPRESSION is the term to substitute into.
VISITED is used internally to track variables during recursive substitution
to prevent infinite loops.

Returns a failure object if BINDINGS is a failure, otherwise returns
the expression with all bound variables replaced by their values.
Recursively processes compound expressions (lists)."
  (cond
   ((eprolog--failure-p bindings) (make-eprolog--failure))
   ((null bindings) expression)
   ((consp expression)
    (cons (eprolog--substitute-bindings bindings (car expression) visited)
          (eprolog--substitute-bindings bindings (cdr expression) visited)))
   ((and (eprolog--variable-p expression)
         (member expression visited))
    expression)
   ((and (eprolog--variable-p expression)
         (assoc expression bindings))
    (let ((value (eprolog--lookup-variable expression bindings)))
      (eprolog--substitute-bindings bindings value (cons expression visited))))
   (t expression)))

(defun eprolog--variables-in (expression)
  "Extract all named variables from EXPRESSION.
Recursively traverses the expression tree to find all Prolog variables.
Anonymous variables ('_') are excluded from the result.

Returns a list of unique named variable symbols found in EXPRESSION.
Variables are deduplicated with the rightmost occurrence preserved."
  (cl-labels ((collect-all-variables (tree)
                (if (atom tree)
                    (list tree)
                  (append (collect-all-variables (car tree))
                          (collect-all-variables (cdr tree))))))
    (let* ((all-atoms (collect-all-variables expression))
           (variables-only (cl-remove-if-not #'eprolog--named-variable-p all-atoms)))
      (cl-remove-duplicates variables-only :from-end t))))

(defun eprolog--replace-anonymous-variables (expression)
  "Replace all anonymous '_' variables in EXPRESSION with unique generated symbols.
Recursively processes the expression tree, generating fresh variable names
using `gensym' for each anonymous variable encountered.

Returns a copy of EXPRESSION with all '_' variables replaced by unique symbols.
This ensures that multiple anonymous variables in the same clause don't unify
with each other."
  (cond
   ((and (symbolp expression) (eq expression '_)) (gensym "_"))
   ((atom expression) expression)
   (t (cons (eprolog--replace-anonymous-variables (car expression))
            (eprolog--replace-anonymous-variables (cdr expression))))))

(defun eprolog--ground-p (term)
  "Check if TERM is fully ground (contain no unbound variables).
TERM is the term to check for groundness.

A term is ground if it contains no variables or all variables in it
are bound to ground terms.  Uses the current binding environment
from `eprolog-current-bindings'.

Returns non-nil if TERM is ground, nil otherwise."
  (let ((resolved-term (eprolog--substitute-bindings eprolog-current-bindings term)))
    (cond
     ((eprolog--variable-p resolved-term) nil)
     ((consp resolved-term)
      (and (eprolog--ground-p (car resolved-term))
           (eprolog--ground-p (cdr resolved-term))))
     (t t))))

;;; Unification System

(defun eprolog--occurs-check-p (variable expression bindings)
  "Check if VARIABLE occurs within EXPRESSION given current BINDINGS.
VARIABLE is the variable to check for.
EXPRESSION is the term to check within.
BINDINGS is the current variable binding environment.

Prevents infinite structures during unification by detecting circular
references.  Returns non-nil if VARIABLE is found within EXPRESSION
\\(directly or through bindings), nil otherwise.  Follows variable
bindings recursively."
  (cond
   ((and (symbolp expression) (eq variable expression)) t)
   ((and (eprolog--variable-p expression) (assoc expression bindings))
    (let ((value (eprolog--lookup-variable expression bindings)))
      (eprolog--occurs-check-p variable value bindings)))
   ((consp expression)
    (or (eprolog--occurs-check-p variable (car expression) bindings)
        (eprolog--occurs-check-p variable (cdr expression) bindings)))
   (t nil)))

(defun eprolog--unify-var (variable value bindings)
  "Unify VARIABLE with VALUE given current BINDINGS.
VARIABLE must be a Prolog variable symbol.
VALUE is the term to unify the variable with.
BINDINGS is the current variable binding environment.

Handles variable-to-variable unification and performs occurs check if enabled.
Returns updated bindings on success, or a failure object on failure.
Used internally by the main `eprolog--unify' function."
  (cond
   ((assoc variable bindings)
    (let ((bound-term (eprolog--lookup-variable variable bindings)))
      (eprolog--unify bound-term value bindings)))
   ((and (eprolog--variable-p value) (assoc value bindings))
    (let ((bound-term (eprolog--lookup-variable value bindings)))
      (eprolog--unify variable bound-term bindings)))
   ((and eprolog-occurs-check (eprolog--occurs-check-p variable value bindings))
    (make-eprolog--failure))
   (t (cons (cons variable value) bindings))))

(defun eprolog--unify (term1 term2 bindings)
  "Unify TERM1 and TERM2 given current BINDINGS.
TERM1 and TERM2 are the terms to unify.
BINDINGS is the current variable binding environment.

Attempts to make the two terms structurally equivalent by finding
appropriate variable bindings.  Handles atoms, variables, and compound
structures recursively.

Returns updated bindings on success, or a failure object on failure.
This is the main unification function implementing the standard Prolog
unification algorithm."
  (cond
   ((eprolog--failure-p bindings) (make-eprolog--failure))
   ((equal term1 term2) bindings)
   ((eprolog--variable-p term1) (eprolog--unify-var term1 term2 bindings))
   ((eprolog--variable-p term2) (eprolog--unify-var term2 term1 bindings))
   ((and (consp term1) (consp term2))
    (let ((car-bindings (eprolog--unify (car term1) (car term2) bindings)))
      (eprolog--unify (cdr term1) (cdr term2) car-bindings)))
   (t (make-eprolog--failure))))

;;; Clause Database Management

(defun eprolog--get-clauses (predicate-symbol)
  "Retrieve all clauses for PREDICATE-SYMBOL from the current clause database.
PREDICATE-SYMBOL is the symbol identifying the predicate.

Returns a list of clauses associated with the predicate, or an empty list
if no clauses are defined for the predicate.  Clauses are returned in the
order they were asserted."
  (let ((entry (assoc predicate-symbol eprolog-clause-database)))
    (if entry (cdr entry) '())))

(defun eprolog--set-clauses (predicate-symbol clauses)
  "Set the CLAUSES for PREDICATE-SYMBOL in the current clause database.
PREDICATE-SYMBOL is the symbol identifying the predicate.
CLAUSES is a list of clause structures to associate with the predicate.

Replaces any existing clauses for the predicate.  This is a destructive
operation that modifies `eprolog-clause-database'.
CLAUSES can also be a function for built-in predicates implemented in Lisp."
  (let* ((cleaned-db (cl-remove predicate-symbol eprolog-clause-database :key #'car))
         (new-db (cons (cons predicate-symbol clauses) cleaned-db)))
    (setq eprolog-clause-database new-db)
    (cdar new-db)))

(defun eprolog--add-clause (clause)
  "Add CLAUSE to the current clause database.
CLAUSE is a clause structure where the first element is the head
and remaining elements form the body.

Appends the clause to the list of existing clauses for the predicate.
The predicate is determined from the head of the clause (first element)."
  (let* ((predicate-symbol (caar clause))
         (current-clauses (eprolog--get-clauses predicate-symbol))
         (new-clauses (append current-clauses (list clause))))
    (eprolog--set-clauses predicate-symbol new-clauses)))

(defun eprolog--remove-clauses-with-arity! (predicate-symbol arity)
  "Remove all clauses for PREDICATE-SYMBOL with specified ARITY.
PREDICATE-SYMBOL is the symbol identifying the predicate.
ARITY is the number of arguments the clauses should have.

Used by the clause definition macros to replace existing clauses when
redefining predicates.  Only removes clauses with exactly the same arity,
preserving clauses with different arities.  This allows for predicate
overloading with different arities."
  (cl-labels ((has-different-arity-p (clause)
                (not (= (length (cdar clause)) arity))))
    (let* ((current-clauses (eprolog--get-clauses predicate-symbol))
           (new-clauses (cl-remove-if-not #'has-different-arity-p current-clauses)))
      (eprolog--set-clauses predicate-symbol new-clauses))))

(defun eprolog--rename-vars (expression)
  "Create a copy of EXPRESSION with all variables renamed to fresh symbols.
EXPRESSION is the term to rename variables in.

Generates new variable names using `gensym\\=' based on the original
variable names.  Used to avoid variable name conflicts when applying
clauses during resolution.  Each variable gets a unique replacement that
preserves the original variable\\='s base name."
  (cl-labels ((make-renaming-pair (variable)
                (let ((var-string (symbol-name variable)))
                  (cons variable (gensym var-string))))
              (symbol-to-string (sym)
                (if (symbolp sym) (symbol-name sym) "")))
    (let* ((variables (eprolog--variables-in expression))
           (alist (mapcar #'make-renaming-pair variables)))
      (cl-sublis alist expression :key #'symbol-to-string :test #'string=))))

;;; Cut & Choice Point Handling

(defun eprolog--wrap-success-with-cut-handler (result tag)
  "Wrap RESULT so that any future cut with TAG is caught and handled.
If RESULT is a success, its continuation is wrapped; wrapping is
applied recursively to all subsequent successes."
  (if (eprolog--success-p result)
      (let ((bindings (eprolog--success-bindings result))
            (cont     (eprolog--success-continuation result)))
        (make-eprolog--success
         :bindings bindings
         :continuation
         (lambda ()
           (condition-case err
               (let ((next (funcall cont)))
                 (eprolog--wrap-success-with-cut-handler next tag))
             (eprolog-cut-exception
              (if (eq tag (plist-get (cdr err) :tag))
                  (eprolog--wrap-success-with-cut-handler
                   (plist-get (cdr err) :value) tag)
                (signal (car err) (cdr err))))))))
    result))

(defun eprolog--call-with-current-choice-point (proc)
  "Execute PROC with a fresh choice point tag for cut handling.
Ensures the cut handler remains active across continuations."
  (let ((tag (gensym "CHOICE-POINT-")))
    (condition-case err
        (let ((result (funcall proc tag)))
          (eprolog--wrap-success-with-cut-handler result tag))
      (eprolog-cut-exception
       (if (eq tag (plist-get (cdr err) :tag))
           (eprolog--wrap-success-with-cut-handler
            (plist-get (cdr err) :value) tag)
         (signal (car err) (cdr err)))))))

(defun eprolog--insert-choice-point (clause choice-point)
  "Insert CHOICE-POINT tag into cut operators (!) within CLAUSE.
CLAUSE is the clause to modify.
CHOICE-POINT is the unique tag to associate with cuts in this clause.

Transforms bare ! atoms and (!) lists to include the choice point tag
for proper cut semantics.  Used to link cuts to their originating choice points."
  (cl-labels ((insert-cut-term (term)
                (cond
                 ((and (consp term) (eq '! (car term)) (cadr term))
                  (error "Invalid choice-point insertion happened"))
                 ((and (consp term) (eq '! (car term))) (list '! choice-point))
                 ((and (atom term) (eq '! term)) (list '! choice-point))
                 (t term))))
    (mapcar #'insert-cut-term clause)))

(defun eprolog--merge-continuations (continuation-a continuation-b)
  "Merge two backtracking continuations into a single continuation.
CONTINUATION-A is the first continuation to try.
CONTINUATION-B is the second continuation to try if the first fails.

Returns a continuation that first tries CONTINUATION-A, and if that fails
or produces no more solutions, tries CONTINUATION-B.  This is the core
backtracking mechanism that enables trying alternative solutions."
  (lambda ()
    (let ((result-a (funcall continuation-a)))
      (if (or (not result-a) (eprolog--failure-p result-a))
          (funcall continuation-b)
        (let* ((bindings (eprolog--success-bindings result-a))
               (result-continuation (eprolog--success-continuation result-a))
               (new-continuation (eprolog--merge-continuations result-continuation continuation-b)))
          (make-eprolog--success :bindings bindings :continuation new-continuation))))))

;;; Proof Engine Core

(defun eprolog--prove-goal-sequence (goals bindings)
  "Prove a sequence of GOALS with the given BINDINGS.
GOALS is a list of goals to prove in sequence.
BINDINGS is the current variable binding environment.

Returns success with terminal continuation if all goals are proven,
otherwise delegates to `eprolog--prove-goal' for the goal sequence.
Base case for empty goal lists returns success with a terminal continuation."
  (cond
   ((eprolog--failure-p bindings) (make-eprolog--failure))
   ((null goals)
    (let ((terminal-cont (lambda () (make-eprolog--failure))))
      (make-eprolog--success :bindings bindings :continuation terminal-cont)))
   (t (eprolog--prove-goal goals bindings))))

(defun eprolog--prove-goal (goals bindings)
  "Prove the first goal in GOALS with the given BINDINGS.
GOALS is a list of goals where the first will be proven.
BINDINGS is the current variable binding environment.

Looks up the predicate handler (clauses or built-in function) and attempts
resolution.  Handles spy tracing and delegates to appropriate resolution method.
This is the core resolution function that drives the proof search."
  (let* ((goal (car goals))
         (remaining-goals (cdr goals))
         (predicate-symbol (if (consp goal) (car goal) goal))
         (predicate-handler (eprolog--get-clauses predicate-symbol))
         (args (if (consp goal) (cdr goal) '())))
    (eprolog--with-spy
     goal bindings
     (lambda ()
       (cond
        ((functionp predicate-handler)
         (let* ((eprolog-remaining-goals remaining-goals)
                (eprolog-current-bindings bindings))
           (apply predicate-handler args)))
        (t (eprolog--search-matching-clauses goals bindings predicate-handler)))))))

(defun eprolog--apply-clause-to-goal (goals bindings clause)
  "Apply CLAUSE to prove the first goal in GOALS with BINDINGS.
GOALS is the list of goals, where the first will be unified with the
clause head.  BINDINGS is the current variable binding environment.
CLAUSE is the clause to apply (head + body).

Attempts to unify the goal with the clause head, then proves the
clause body plus remaining goals if unification succeeds.
Returns success object on success, failure object on failure."
  (let* ((goal (car goals))
         (remaining-goals (cdr goals))
         (goal-for-unify (if (consp goal) goal (list goal)))
         (renamed-clause (eprolog--rename-vars clause))
         (clause-head (car renamed-clause))
         (clause-body (cdr renamed-clause))
         (new-bindings (eprolog--unify goal-for-unify clause-head bindings)))
    (if (eprolog--failure-p new-bindings)
        (make-eprolog--failure)
      (eprolog--prove-goal-sequence (append clause-body remaining-goals) new-bindings))))

(defun eprolog--search-matching-clauses (goals bindings all-clauses)
  "Search through ALL-CLAUSES to find matches for the first goal in GOALS.
GOALS is the list of goals to prove.
BINDINGS is the current variable binding environment.
ALL-CLAUSES is the list of clauses to try.

Tries each clause with backtracking.  Uses choice points to handle cut
operations correctly.  Returns success object with continuation for
backtracking, or failure if no clauses match."
  (eprolog--call-with-current-choice-point
   (lambda (choice-point)
     (cl-labels ((try-one-by-one (clauses-to-try)
                   (if (null clauses-to-try)
                       (make-eprolog--failure)
                     (let* ((current-clause (eprolog--insert-choice-point (car clauses-to-try) choice-point))
                            (remaining-clauses (cdr clauses-to-try))
                            (try-next-clause (lambda () (try-one-by-one remaining-clauses)))
                            (result (eprolog--apply-clause-to-goal goals bindings current-clause)))
                       (if (eprolog--failure-p result)
                           (funcall try-next-clause)
                         (let* ((result-bindings (eprolog--success-bindings result))
                                (result-continuation (eprolog--success-continuation result))
                                (new-continuation (eprolog--merge-continuations result-continuation try-next-clause)))
                           (make-eprolog--success :bindings result-bindings :continuation new-continuation)))))))
       (try-one-by-one all-clauses)))))

;;; Debugging & Output

(defun eprolog--printf (format &rest args)
  "Print formatted output using FORMAT and ARGS.
Wrapper around `format' and `princ' for consistent output handling
throughout the Prolog engine."
  (princ (apply #'format format args)))

(defun eprolog--spy-prompt (goal bindings)
  "Display an interactive spy prompt for GOAL with current BINDINGS.
GOAL is the goal being traced.
BINDINGS is the current variable binding environment.

Shows the resolved goal and prompts for spy action.

Returns non-nil if execution should continue, nil to abort."
  (let ((resolved (eprolog--substitute-bindings bindings goal)))
    (message "SPY: %S (press any key to continue)" resolved) (read-char) t))

(defun eprolog--spy-message (kind goal bindings)
  "Display a spy trace message of KIND for GOAL with BINDINGS.
KIND is typically \\='CALL\\=', \\='EXIT\\=', or \\='FAIL\\='.
GOAL is the goal being traced.
BINDINGS is the current variable binding environment.

Shows the resolved goal after applying current variable bindings."
  (let ((resolved (eprolog--substitute-bindings bindings goal)))
    (eprolog--printf "\\n** %s: %S" kind resolved)))

(defun eprolog--with-spy (goal bindings thunk)
  "Execute THUNK with spy tracing for GOAL using BINDINGS.
GOAL is the goal to trace.
BINDINGS is the current variable binding environment.
THUNK is a function to execute while tracing.

Checks if the goal's predicate is being spied on and shows appropriate
trace messages.  Handles spy mode settings and displays CALL/EXIT/FAIL traces.
Returns the result of executing THUNK."
  (let* ((predicate-symbol (if (consp goal) (car goal) goal))
         (spy-p (member predicate-symbol eprolog-spy-predicates))
         (show-p (and spy-p
                      (pcase eprolog-spy-state
                        ('always t)
                        ('prompt (eprolog--spy-prompt goal bindings))
                        (_ nil))))
         (result nil))
    (unwind-protect
        (progn
          (when show-p
            (eprolog--spy-message "CALL" goal bindings))
          (setq result (funcall thunk)))
      (when show-p
        (if (or (not result) (eprolog--failure-p result))
            (eprolog--spy-message "FAIL" goal bindings)
          (eprolog--spy-message "EXIT" goal (eprolog--success-bindings result)))))
    result))

(defun eprolog--display-solution (bindings)
  "Display variable BINDINGS as a solution.
BINDINGS is an alist of variable-value pairs.

Shows \\='Yes\\=' for solutions with no variable bindings (pure facts),
otherwise displays each variable-value pair on separate lines."
  (if (null bindings)
      (eprolog--printf "\\nYes")
    (dolist (var-val bindings)
      (eprolog--printf "\\n%s = %s" (car var-val) (cdr var-val)))))

(defun eprolog--run (goals)
  "Execute GOALS as an interactive query with user continuation prompt.
GOALS is the list of goals to execute.

Displays each solution and prompts whether to continue searching for more.
Shows \\='No\\=' if no solutions exist.  This is the main entry point for
interactive queries initiated by `eprolog-query\\='."
  (cl-block query-exit
    (eprolog-solve
     goals
     :success
     (lambda (solution)
       (eprolog--display-solution solution)
       (unless (y-or-n-p "Continue?")
         (cl-return-from query-exit)))
     :failure
     (lambda ()
       (eprolog--printf "\\nNo")
       (cl-return-from query-exit)))))

;;; Lisp Integration Helper

(defun eprolog--eval-lisp-expressions (expressions &optional result-handler)
  "Evaluate EXPRESSIONS as Lisp code with unified safety checks and handling.

This is a helper function used by the Lisp evaluation predicates (lisp/2+,
lisp!/1+, lispp/1+) to consolidate common functionality.

Arguments:
  EXPRESSIONS - List of Lisp expressions to evaluate
  RESULT-HANDLER - Optional function to process evaluation result

Behavior:
  - Checks that EXPRESSIONS are fully ground (no unbound variables)
  - Substitutes current variable bindings into expressions
  - Evaluates expressions as (progn ,@expressions)
  - If RESULT-HANDLER provided, calls it with evaluation result
  - Otherwise continues proof with current bindings
  - Returns appropriate success/failure object for proof engine

Safety:
  - Fails immediately if expressions contain unbound variables
  - Proper integration with eprolog proof engine and backtracking"
  (if (not (eprolog--ground-p expressions))
      (make-eprolog--failure)
    (let* ((lisp-expression (eprolog--substitute-bindings eprolog-current-bindings `(progn ,@expressions)))
           (evaluated-result (eval lisp-expression)))
      (if result-handler
          (funcall result-handler evaluated-result)
        (eprolog--prove-goal-sequence eprolog-remaining-goals eprolog-current-bindings)))))

;;; Public API

(defun eprolog-solve (goals &rest args)
  "Solve GOALS and call callbacks for each solution.
GOALS is the list of goals to solve.

Optional keyword arguments (ARGS):
:success ON-SUCCESS -
  Function called with variable bindings for each solution found.
  Defaults to a no-op function.
:failure ON-FAILURE -
  Function called once when no more solutions exist.
  Defaults to a no-op function.

This is the core solution iterator that drives the proof search and handles
solution enumeration through backtracking.

Examples:
  (eprolog-solve \\='((parent tom _x)))
  (eprolog-solve \\='((parent tom _x)) :success #\\='print)
  (eprolog-solve \\='((parent tom _x))
    :success (lambda (bindings) (message \\\"Found: %S\\\" bindings))
    :failure (lambda () (message \\\"No more solutions\\\")))"
  (let* ((on-success (or (plist-get args :success) (lambda (_))))
         (on-failure (or (plist-get args :failure) (lambda ()))))
    (cl-labels ((initial-continuation ()
                  (eprolog--call-with-current-choice-point
                   (lambda (_choice-point)
                     (let* ((prepared-goals (eprolog--replace-anonymous-variables goals)))
                       (eprolog--prove-goal-sequence prepared-goals '())))))
                (retrieve-success-bindings (result)
                  (let* ((bindings (eprolog--success-bindings result))
                         (query-variables (eprolog--variables-in goals))
                         (make-binding-pair (lambda (v) (cons v (eprolog--substitute-bindings bindings v)))))
                    (mapcar make-binding-pair query-variables)))
                (execute-success-continuation (result)
                  (funcall (eprolog--success-continuation result))))
      (cl-do ((result (initial-continuation) (execute-success-continuation result)))
          ((not (eprolog--success-p result)) (funcall on-failure))
        (funcall on-success (retrieve-success-bindings result))))))

(defmacro eprolog-define-lisp-predicate (name args &rest body)
  "Define a predicate implemented as an Emacs Lisp function.
NAME is the predicate symbol.
ARGS is the list of formal parameters for the predicate.
BODY is the Lisp implementation that should return a success or failure
object.

The predicate can access `eprolog-current-bindings\\=' and
`eprolog-remaining-goals\\=' to interact with the proof engine.
Built-in predicates use this macro."
  (declare (indent defun))
  `(eprolog--set-clauses ',name (lambda ,args ,@body)))

(defmacro eprolog-define-prolog-predicate (head &rest body)
  "Define a Prolog clause (fact or rule) and add it to the clause database.
HEAD is the predicate head, a list of (NAME . ARGS), or a symbol for
predicates with no arguments.
BODY is the optional list of goals forming the rule body.

If BODY is empty, defines a fact.  If BODY is non-empty, defines a rule.
Anonymous variables are automatically replaced with unique variables.

Example: (eprolog-define-prolog-predicate (parent tom bob))
Example: (eprolog-define-prolog-predicate (grandparent _x _z)
          (parent _x _y) (parent _y _z))
Example: (eprolog-define-prolog-predicate true)"
  (declare (indent defun))
  (let* ((head-list (if (consp head) head (list head)))
         (clause (cons head-list body)))
    `(eprolog--add-clause (eprolog--replace-anonymous-variables ',clause))))

(defmacro eprolog-define-prolog-predicate! (head &rest body)
  "Define a Prolog clause, replacing existing clauses with the same arity.
HEAD is the predicate head, a list of (NAME . ARGS), or a symbol for
predicates with no arguments.
BODY is the optional list of goals forming the rule body.

Similar to `eprolog-define-prolog-predicate\\=' but removes existing
clauses for the predicate with the same arity before adding the new
clause.  Used for predicate redefinition.

Example: (eprolog-define-prolog-predicate! (factorial 0 1))
Example: (eprolog-define-prolog-predicate! true)
  replaces all factorial/2 clauses."
  (declare (indent defun))
  (let* ((head-list (if (consp head) head (list head)))
         (name (car head-list))
         (args (cdr head-list))
         (arity (length args))
         (clause (cons head-list body)))
    `(progn
       (eprolog--remove-clauses-with-arity! ',name ,arity)
       (eprolog--add-clause (eprolog--replace-anonymous-variables ',clause)))))

(defmacro eprolog-query (&rest goals)
  "Execute a Prolog query interactively.
GOALS is a list of goals to prove.

Displays solutions and prompts for continuation after each solution.
Anonymous variables are automatically replaced with unique variables.
Shows variable bindings for each solution found.

Example: (eprolog-query (parent _x _y)) finds all parent relationships."
  `(eprolog--run (eprolog--replace-anonymous-variables ',goals)))

;; Aliases for convenience
(defalias 'eprolog-define-predicate 'eprolog-define-prolog-predicate)
(defalias 'eprolog-define-predicate! 'eprolog-define-prolog-predicate!)

;;; Built-in Lisp Predicates

;; Unification predicates
(eprolog-define-lisp-predicate = (term1 term2)
  "Unification predicate: TERM1 = TERM2.
Attempts to unify TERM1 and TERM2, updating the binding environment.
Succeeds if unification is possible, fails otherwise."
  (let ((new-bindings (eprolog--unify term1 term2 eprolog-current-bindings)))
    (if (eprolog--failure-p new-bindings)
        (make-eprolog--failure)
      (eprolog--prove-goal-sequence eprolog-remaining-goals new-bindings))))

(eprolog-define-lisp-predicate == (term1 term2)
  "Strict equality predicate: TERM1 == TERM2.
Tests if TERM1 and TERM2 are identical after variable substitution.
Does not perform unification, only tests existing equality."
  (let* ((substituted-term1 (eprolog--substitute-bindings eprolog-current-bindings term1))
         (substituted-term2 (eprolog--substitute-bindings eprolog-current-bindings term2)))
    (if (equal substituted-term1 substituted-term2)
        (eprolog--prove-goal-sequence eprolog-remaining-goals eprolog-current-bindings)
      (make-eprolog--failure))))

;; Control predicates
(eprolog-define-lisp-predicate ! (choice-point)
  "Cut predicate: !.
Commits to the current choice, preventing backtracking to alternative clauses
for the current goal. CHOICE-POINT identifies the choice point to cut."
  (let ((continuation-result (eprolog--prove-goal-sequence eprolog-remaining-goals eprolog-current-bindings)))
    (signal 'eprolog-cut-exception (list :tag choice-point :value continuation-result))))

(eprolog-define-lisp-predicate call (pred &rest args)
  "Meta-call predicate: call(PRED, ARGS...).
Dynamically calls PRED with additional ARGS appended.
PRED can be an atom or a compound term."
  (eprolog--call-with-current-choice-point
   (lambda (choice-point)
     (let* ((substituted-pred (eprolog--substitute-bindings eprolog-current-bindings pred))
            (substituted-args (eprolog--substitute-bindings eprolog-current-bindings args))
            (goal (cond
                   ((null substituted-args) substituted-pred)
                   ((symbolp substituted-pred) (cons substituted-pred substituted-args))
                   ((consp substituted-pred) (append substituted-pred substituted-args))
                   (t (error "call: Invalid form %S" (cons substituted-pred substituted-args)))))
            (cut-goals (eprolog--insert-choice-point (list goal) choice-point))
            (next-goals (append cut-goals eprolog-remaining-goals)))
       (eprolog--prove-goal-sequence next-goals eprolog-current-bindings)))))

(eprolog-define-lisp-predicate var (term)
  "Type predicate: var(TERM).
Succeeds if TERM is an unbound variable."
  (if (eprolog--variable-p (eprolog--substitute-bindings eprolog-current-bindings term))
      (eprolog--prove-goal-sequence eprolog-remaining-goals eprolog-current-bindings)
    (make-eprolog--failure)))

;; Lisp integration predicates
(eprolog-define-lisp-predicate lisp (result-variable &rest expressions)
  "Lisp evaluation predicate: lisp(RESULT, EXPRESSIONS...).

Evaluates EXPRESSIONS as Lisp code and unifies the result with RESULT-VARIABLE.

Usage:
  (eprolog-query (lisp X (+ 2 3)))           ; X = 5
  (eprolog-query (lisp Y (length '(a b c)))) ; Y = 3
  (eprolog-query (lisp Z (current-buffer)))  ; Z = #<buffer ...>

Behavior:
  - All expressions must be ground (fully instantiated)
  - Expressions evaluated as (progn Expression1 Expression2 ...)
  - Final result unified with RESULT-VARIABLE
  - Succeeds if unification succeeds, fails otherwise
  - Integrates with backtracking and choice points

See also: lisp!/1+ for side effects, lispp/1+ for boolean tests."
  (eprolog--eval-lisp-expressions
   expressions
   (lambda (evaluated-result)
     (let* ((result-term (eprolog--substitute-bindings eprolog-current-bindings result-variable))
            (new-bindings (eprolog--unify result-term evaluated-result eprolog-current-bindings)))
       (if (eprolog--failure-p new-bindings)
           (make-eprolog--failure)
         (eprolog--prove-goal-sequence eprolog-remaining-goals new-bindings))))))

(eprolog-define-lisp-predicate lisp! (&rest expressions)
  "Lisp evaluation predicate: lisp!(EXPRESSIONS...).

Evaluates EXPRESSIONS as Lisp code for side effects, always succeeds.

Usage:
  (eprolog-query (lisp! (message \\\"Hello from Prolog!\\\")))
  (eprolog-query (lisp! (setq my-var 42) (push 'item my-list)))

Behavior:
  - All expressions must be ground (fully instantiated)
  - Expressions evaluated as (progn Expression1 Expression2 ...)
  - Return value is ignored
  - Always succeeds and continues proof
  - Useful for side effects: I/O, variable assignment, etc.

See also: lisp/2+ for capturing results, lispp/1+ for boolean tests."
  (eprolog--eval-lisp-expressions expressions))

(eprolog-define-lisp-predicate lispp (&rest expressions)
  "Lisp evaluation predicate: lispp(EXPRESSIONS...).

Evaluates EXPRESSIONS as Lisp code and succeeds if the result is non-nil.

Usage:
  (eprolog-query (lispp (> X 0)))             ; succeeds if X > 0
  (eprolog-query (lispp (member Item List)))  ; succeeds if Item in List
  (eprolog-query (lispp (file-exists-p \\\"path\\\")))

Behavior:
  - All expressions must be ground (fully instantiated)
  - Expressions evaluated as (progn Expression1 Expression2 ...)
  - Succeeds if final result is non-nil
  - Fails if final result is nil
  - Used for Lisp-based conditions in Prolog rules

Examples in rules:
  (eprolog-assert! (positive X) (lispp (> X 0)))
  (eprolog-assert! (file-exists Path) (lispp (file-exists-p Path)))

See also: lisp/2+ for capturing results, lisp!/1+ for side effects."
  (eprolog--eval-lisp-expressions
   expressions
   (lambda (evaluated-result)
     (if (not evaluated-result)
         (make-eprolog--failure)
       (eprolog--prove-goal-sequence eprolog-remaining-goals eprolog-current-bindings)))))

;; Dynamic parameter predicates
(eprolog-define-lisp-predicate dynamic-put (variable-symbol value-expression)
  "Dynamic parameter predicate: dynamic-put(SYMBOL, VALUE).
Stores VALUE under SYMBOL in the dynamic parameter store.
VALUE-EXPRESSION is evaluated as Lisp code before storing."
  (let* ((substituted-expression (eprolog--substitute-bindings eprolog-current-bindings value-expression))
         (evaluated-value (eval substituted-expression))
         (eprolog-dynamic-parameters
          (cons (cons variable-symbol evaluated-value) eprolog-dynamic-parameters)))
    (eprolog--prove-goal-sequence eprolog-remaining-goals eprolog-current-bindings)))

(eprolog-define-lisp-predicate dynamic-get (variable-symbol prolog-variable)
  "Dynamic parameter predicate: dynamic-get(SYMBOL, VAR).
Retrieves the value associated with SYMBOL and unifies it with VAR."
  (let* ((key-value (assoc variable-symbol eprolog-dynamic-parameters))
         (value (if key-value (cdr key-value) nil))
         (new-bindings (eprolog--unify prolog-variable value eprolog-current-bindings)))
    (if (eprolog--failure-p new-bindings)
        (make-eprolog--failure)
      (eprolog--prove-goal-sequence eprolog-remaining-goals new-bindings))))

;;; Built-in Prolog Predicates

;; Type checking predicates
(eprolog-define-prolog-predicate! (atom _term)
  (lispp (atom '_term)))

(eprolog-define-prolog-predicate! (atomic _term)
  (lispp (not (consp '_term))))

(eprolog-define-prolog-predicate! (number _term)
  (lispp (numberp '_term)))

(eprolog-define-prolog-predicate! (string _term)
  (lispp (stringp '_term)))

(eprolog-define-prolog-predicate! (ground _term)
  (lisp! (list '_term)))

(eprolog-define-prolog-predicate! (fail)
  (lisp! (list '_)))

;; Basic logical predicates
(eprolog-define-prolog-predicate! true)
(eprolog-define-prolog-predicate! false (fail))

(eprolog-define-prolog-predicate! (not _goal)
  (call _goal) ! (fail))
(eprolog-define-prolog-predicate (not _goal))

;; Logical operators
(eprolog-define-prolog-predicate! and (true))
(eprolog-define-prolog-predicate! (and _x1)
  (call _x1))
(eprolog-define-prolog-predicate! (and _x1 _x2)
  (and _x1) (and _x2))
(eprolog-define-prolog-predicate! (and _x1 _x2 _x3)
  (and _x1 (and _x2 _x3)))
(eprolog-define-prolog-predicate! (and _x1 _x2 _x3 _x4)
  (and _x1 (and _x2 _x3 _x4)))

(eprolog-define-prolog-predicate! or (fail))
(eprolog-define-prolog-predicate (or _x1)
  (call _x1))
(eprolog-define-prolog-predicate (or _x1 _x2)
  (or _x1))
(eprolog-define-prolog-predicate (or _x1 _x2)
  (or _x2))
(eprolog-define-prolog-predicate (or _x1 _x2 _x3)
  (or _x1 (or _x2 _x3)))
(eprolog-define-prolog-predicate (or _x1 _x2 _x3 _x4)
  (or _x1 (or _x2 _x3 _x4)))

(eprolog-define-prolog-predicate! (if _cond _then)
  (call _cond) (call _then))
(eprolog-define-prolog-predicate! (if _cond _then _else)
  (call _cond) ! (call _then))
(eprolog-define-prolog-predicate (if _cond _then _else)
  (call _else))

;; Arithmetic
(eprolog-define-prolog-predicate! (is _result _expression)
  (lisp _result _expression))

;; Control flow
(eprolog-define-prolog-predicate! repeat)
(eprolog-define-prolog-predicate (repeat) (repeat))

;; List operations
(eprolog-define-prolog-predicate! (member _item (_item . _)))
(eprolog-define-prolog-predicate (member _item (_ . _rest)) (member _item _rest))

(eprolog-define-prolog-predicate! (append () _list _list))
(eprolog-define-prolog-predicate (append (_head . _tail) _list (_head . _result))
  (append _tail _list _result))

(eprolog-define-prolog-predicate! (append () ()))
(eprolog-define-prolog-predicate (append (_head . _tail) _result)
  (append _tail _tail-result)
  (append _head _tail-result _result))

;; Higher-order predicates
(eprolog-define-prolog-predicate! (maplist _goal ()))
(eprolog-define-prolog-predicate (maplist _goal (_head . _tail))
  (call _goal _head)
  (maplist _goal _tail))

(eprolog-define-prolog-predicate! (maplist _goal () ()))
(eprolog-define-prolog-predicate (maplist _goal (_head1 . _tail1) (_head2 . _tail2))
  (call _goal _head1 _head2)
  (maplist _goal _tail1 _tail2))

(eprolog-define-prolog-predicate! (maplist _goal () () ()))
(eprolog-define-prolog-predicate (maplist _goal (_head1 . _tail1) (_head2 . _tail2) (_head3 . _tail3))
  (call _goal _head1 _head2 _head3)
  (maplist _goal _tail1 _tail2 _tail3))

(eprolog-define-prolog-predicate! (maplist _goal () () () ()))
(eprolog-define-prolog-predicate (maplist _goal (_head1 . _tail1) (_head2 . _tail2) (_head3 . _tail3) (_head4 . _tail4))
  (call _goal _head1 _head2 _head3 _head4)
  (maplist _goal _tail1 _tail2 _tail3 _tail4))

;;; DCG (Definite Clause Grammar) Support

(defun eprolog--transform-dcg-body (body in-var out-var)
  "Transform a DCG BODY into Prolog goals with difference lists.
BODY is the list of DCG elements to transform.
IN-VAR is the input list variable.
OUT-VAR is the output list variable.

Returns a list of Prolog goals implementing the DCG body using difference lists.
Handles terminals (strings), non-terminals (symbols), semantic actions (@),
cuts (!), and epsilon (empty) productions."
  (if (null body)
      nil
    (let* ((element (car body))
           (rest (cdr body))
           (next-var (if rest (gensym "_") out-var)))
      (cond
       ;; Handle epsilon (empty production)
       ((null element)
        ;; epsilon means no consumption, so continue with same variables
        (let ((match-goal `(= ,in-var ,next-var)))
          (cons match-goal (eprolog--transform-dcg-body rest in-var out-var))))

       ;; Handle cut (!) - pass through as-is, doesn't consume input
       ((eq element '!)
        (cons element (eprolog--transform-dcg-body rest in-var out-var)))

       ;; Handle variables (pass through as-is)
       ((eprolog--variable-p element)
        (let ((match-goal `(= ,in-var (,element . ,next-var))))
          (cons match-goal (eprolog--transform-dcg-body rest next-var out-var))))

       ;; Handle strings - terminals
       ((stringp element)
        ;; Keep string as-is for terminal matching
        (let ((match-goal `(= ,in-var (,element . ,next-var))))
          (cons match-goal (eprolog--transform-dcg-body rest next-var out-var))))

       ;; Cut as list: (!)
       ((and (consp element) (eq (car element) '!) (null (cdr element)))
        (cons '! (eprolog--transform-dcg-body rest in-var out-var)))

       ;; Semantic actions: (@ goal...)
       ((and (consp element) (eq (car element) '@))
        (let ((goals (cdr element)))
          (append `((= ,in-var ,next-var))
                  goals
                  (eprolog--transform-dcg-body rest in-var out-var))))

       ;; Non-terminals with arguments (lists)
       ((consp element)
        (let* ((pred-name (car element))
               (pred-args (cdr element))
               (dcg-call `(call ,pred-name ,@pred-args ,in-var ,next-var)))
          (cons dcg-call (eprolog--transform-dcg-body rest next-var out-var))))

       ;; Non-terminals (symbols)
       ((symbolp element)
        (let ((dcg-call `(call ,element ,in-var ,next-var)))
          (cons dcg-call (eprolog--transform-dcg-body rest next-var out-var))))

       ;; Default case
       (t (error "Invalid DCG body element: %S" element))))))

(defmacro eprolog-define-grammar! (&rest dcg-parts)
  "Define a DCG rule (DCG-PARTS), replacing existing rules with the same arity.
Syntax: (eprolog-define-grammar head body1 body2 ...)
     or (eprolog-define-grammar (head args...) body1 body2 ...)

Transforms DCG notation into standard Prolog clauses with difference lists.

DCG Conventions:
- Strings (\\\"word\\\") are terminals
- Symbols (word) are non-terminals
- Lists ((word args...)) are non-terminals with arguments
- nil represents empty production (epsilon)
- (@ goal...) are semantic actions (constraints that don\\='t consume input)
- ! is cut (prevents backtracking)

Examples:
  (eprolog-define-grammar s np vp)
  (eprolog-define-grammar (s _x) (np _x) (vp _x))
  (eprolog-define-grammar noun \\\"cat\\\")
  (eprolog-define-grammar optional nil)
  (eprolog-define-grammar (s _num) (@ (= _num 3)) (np _num) (vp _num))
  ; With constraint"
  (let* ((head (car dcg-parts))
         (body (cdr dcg-parts))
         (head-name (if (consp head) (car head) head))
         (head-args (if (consp head) (cdr head) '()))
         (in-var (gensym "_"))
         (out-var (gensym "_"))
         (transformed-args `(,@head-args ,in-var ,out-var))
         (transformed-body (eprolog--transform-dcg-body body in-var out-var)))
    `(eprolog-define-prolog-predicate! (,head-name ,@transformed-args) ,@transformed-body)))

(defmacro eprolog-define-grammar (&rest dcg-parts)
  "Define a DCG rule (DCG-PARTS), adding to existing rules with the same arity.
Syntax: (eprolog-add-grammar head body1 body2 ...)
     or (eprolog-add-grammar (head args...) body1 body2 ...)

Similar to `eprolog-define-grammar!\\=' but adds clauses without replacing
existing ones.  This allows multiple DCG rules for the same non-terminal.

DCG Conventions:
- Strings (\\\"word\\\") are terminals
- Symbols (word) are non-terminals
- Lists ((word args...)) are non-terminals with arguments
- nil represents empty production (epsilon)
- (@ goal...) are semantic actions (constraints that don\\='t consume input)
- ! is cut (prevents backtracking)

Examples:
  (eprolog-define-grammar noun \\\"cat\\\")
  (eprolog-define-grammar noun \\\"dog\\\")
  (eprolog-define-grammar (det _num) \\\"the\\\")"
  (let* ((head (car dcg-parts))
         (body (cdr dcg-parts))
         (head-name (if (consp head) (car head) head))
         (head-args (if (consp head) (cdr head) '()))
         (in-var (gensym "_"))
         (out-var (gensym "_"))
         (transformed-args `(,@head-args ,in-var ,out-var))
         (transformed-body (eprolog--transform-dcg-body body in-var out-var)))
    `(eprolog-define-prolog-predicate (,head-name ,@transformed-args) ,@transformed-body)))

;; DCG parsing predicates
(eprolog-define-prolog-predicate! (phrase _NonTerminal _List)
  (call _NonTerminal _List nil))

(eprolog-define-prolog-predicate (phrase _NonTerminal _List _Rest)
  (call _NonTerminal _List _Rest))

(provide 'eprolog)
;;; eprolog.el ends here
