;;; eprolog.el --- Native Prolog engine implementation -*- lexical-binding: t -*-

;; We acknowledge that this code is heavily inspired by Peter Norvig's
;; "Paradigms of Artificial Intelligence Programming" Prolog implementation,
;; which are lisenced under MIT License.
;; Copyright (C) 2018 Peter Norvig
;;
;; However, this implementation is a complete rewrite in Emacs Lisp with
;; significant modifications and extensions to support a full Prolog system
;; integrated into Emacs.
;; Copyright (C) 2025 Masaya Taniguchi

;; Author: Masaya Taniguchi
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.2"))
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
(define-error 'eprolog-step-limit-exceeded
  "Prolog engine step limit exceeded"
  'error)

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
- \='prompt: Ask user for each spy action
- \='always: Show all spy messages without prompting
- \='disabled: No spy output")

(defvar eprolog-occurs-check t
  "Whether to perform occurs check during unification.
When non-nil, prevents creation of infinite structures by checking
if a variable occurs within the term it's being unified with.")

(defvar eprolog-max-steps 10000
  "Maximum number of engine dispatch steps for a single solve.
Nil disables the limit.  This prevents runaway non-terminating queries
from hanging Emacs indefinitely.  Reaching the limit signals
`eprolog-step-limit-exceeded'.")

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
An alist of `symbol-value' pairs used by the store and fetch
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

(defun eprolog--dereference-term (term bindings &optional visited)
  "Follow variable bindings for TERM within BINDINGS.
VISITED prevents infinite loops in cyclic binding chains."
  (let ((value term)
        (seen visited))
    (while (and (eprolog--variable-p value)
                (not (member value seen))
                (assoc value bindings))
      (push value seen)
      (setq value (eprolog--lookup-variable value bindings)))
    value))

(defun eprolog--set-cons-slot (cell slot value)
  "Store VALUE into CELL's SLOT."
  (if (eq slot 'car)
      (setcar cell value)
    (setcdr cell value)))

(defun eprolog--substitute-bindings (bindings expression &optional visited)
  "Apply variable BINDINGS to EXPRESSION, replacing bound variables.
BINDINGS is an alist of variable-value pairs, or a failure object.
EXPRESSION is the term to substitute into.
VISITED is used internally to track variables during recursive substitution
to prevent infinite loops.

Returns a failure object if BINDINGS is a failure, otherwise returns
the expression with all bound variables replaced by their values.
Recursively processes compound expressions (lists)."
  (pcase bindings
    ((pred eprolog--failure-p) (make-eprolog--failure))
    ('nil expression)
    (_
     (let* ((root (cons nil nil))
            (worklist (list (list expression root 'car visited))))
       (while worklist
         (pcase-let ((`(,current ,parent ,slot ,seen) (pop worklist)))
           (setq current (eprolog--dereference-term current bindings seen))
           (if (consp current)
               (let ((node (cons nil nil)))
                 (eprolog--set-cons-slot parent slot node)
                 (push (list (cdr current) node 'cdr seen) worklist)
                 (push (list (car current) node 'car seen) worklist))
             (eprolog--set-cons-slot parent slot current))))
       (car root)))))

(defun eprolog--variables-in (expression)
  "Extract all named variables from EXPRESSION.
Traverses the expression tree to find all Prolog variables.
Anonymous variables ('_') are excluded from the result.

Returns a list of unique named variable symbols found in EXPRESSION.
Variables are deduplicated with the rightmost occurrence preserved."
  (let ((worklist (list expression))
        (all-atoms '()))
    (while worklist
      (let ((current (pop worklist)))
        (if (atom current)
            (push current all-atoms)
          (push (cdr current) worklist)
          (push (car current) worklist))))
    (let* ((variables-only (cl-remove-if-not #'eprolog--named-variable-p
                                             (nreverse all-atoms))))
      (cl-remove-duplicates variables-only :from-end t))))

(defun eprolog--replace-anonymous-variables (expression)
  "Replace all anonymous '_' variables in EXPRESSION with unique generated symbols.
Recursively processes the expression tree, generating fresh variable names
using `gensym' for each anonymous variable encountered.

Returns a copy of EXPRESSION with all '_' variables replaced by unique symbols.
This ensures that multiple anonymous variables in the same clause don't unify
with each other."
  (let* ((root (cons nil nil))
         (worklist (list (list expression root 'car))))
    (while worklist
      (pcase-let ((`(,current ,parent ,slot) (pop worklist)))
        (cond
         ((eq current '_)
          (eprolog--set-cons-slot parent slot (gensym "_")))
         ((consp current)
          (let ((node (cons nil nil)))
            (eprolog--set-cons-slot parent slot node)
            (push (list (cdr current) node 'cdr) worklist)
            (push (list (car current) node 'car) worklist)))
         (t
          (eprolog--set-cons-slot parent slot current)))))
    (car root)))

(defun eprolog--ground-p (term)
  "Check if TERM is fully ground (contain no unbound variables).
TERM is the term to check for groundness.

A term is ground if it contains no variables or all variables in it
are bound to ground terms.  Uses the current binding environment
from `eprolog-current-bindings'.

Returns non-nil if TERM is ground, nil otherwise."
  (let ((worklist (list (eprolog--substitute-bindings eprolog-current-bindings term)))
        ground)
    (setq ground t)
    (while (and worklist ground)
      (let ((current (pop worklist)))
        (cond
         ((eprolog--variable-p current)
          (setq ground nil))
         ((consp current)
          (push (cdr current) worklist)
          (push (car current) worklist)))))
    ground))

;;; Unification System

(defun eprolog--occurs-check-p (variable expression bindings)
  "Check if VARIABLE occurs within EXPRESSION given current BINDINGS.
VARIABLE is the variable to check for.
EXPRESSION is the term to check within.
BINDINGS is the current variable binding environment.

Prevents infinite structures during unification by detecting circular
references.  Returns non-nil if VARIABLE is found within EXPRESSION
\(directly or through bindings), nil otherwise.  Follows variable
bindings recursively."
  (let ((worklist (list expression))
        (visited-vars '())
        found)
    (while (and worklist (not found))
      (let ((current (pop worklist)))
        (cond
         ((eq variable current)
          (setq found t))
         ((eprolog--variable-p current)
          (unless (member current visited-vars)
            (push current visited-vars)
            (when (assoc current bindings)
              (push (eprolog--lookup-variable current bindings) worklist))))
         ((consp current)
          (push (cdr current) worklist)
          (push (car current) worklist)))))
    found))

(defun eprolog--unify-var (variable value bindings)
  "Unify VARIABLE with VALUE given current BINDINGS.
VARIABLE must be a Prolog variable symbol.
VALUE is the term to unify the variable with.
BINDINGS is the current variable binding environment.

Handles variable-to-variable unification and performs occurs check if enabled.
Returns updated bindings on success, or a failure object on failure.
Used internally by the main `eprolog--unify' function."
  (let ((resolved-var (eprolog--dereference-term variable bindings))
        (resolved-value (eprolog--dereference-term value bindings)))
    (cond
     ((equal resolved-var resolved-value) bindings)
     ((not (eprolog--variable-p resolved-var))
      (eprolog--unify resolved-var resolved-value bindings))
     ((and eprolog-occurs-check
           (eprolog--occurs-check-p resolved-var resolved-value bindings))
      (make-eprolog--failure))
     (t
      (cons (cons resolved-var resolved-value) bindings)))))

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
  (pcase bindings
    ((pred eprolog--failure-p) (make-eprolog--failure))
    (_
     (let ((pending (list (cons term1 term2)))
           (current-bindings bindings)
           failed)
       (while (and pending (not failed))
         (pcase-let ((`(,left . ,right) (pop pending)))
           (setq left (eprolog--dereference-term left current-bindings))
           (setq right (eprolog--dereference-term right current-bindings))
           (cond
            ((equal left right))
            ((eprolog--variable-p left)
             (setq current-bindings (eprolog--unify-var left right current-bindings))
             (when (eprolog--failure-p current-bindings)
               (setq failed t)))
            ((eprolog--variable-p right)
             (setq current-bindings (eprolog--unify-var right left current-bindings))
             (when (eprolog--failure-p current-bindings)
               (setq failed t)))
            ((and (consp left) (consp right))
             (push (cons (cdr left) (cdr right)) pending)
             (push (cons (car left) (car right)) pending))
            (t
             (setq current-bindings (make-eprolog--failure))
             (setq failed t)))))
       current-bindings))))

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

Generates new variable names using `gensym\=' based on the original
variable names.  Used to avoid variable name conflicts when applying
clauses during resolution.  Each variable gets a unique replacement that
preserves the original variable\='s base name."
  (cl-labels ((make-renaming-pair (variable)
                (let ((var-string (symbol-name variable)))
                  (cons variable (gensym var-string))))
              (symbol-to-string (sym)
                (if (symbolp sym) (symbol-name sym) "")))
    (let* ((variables (eprolog--variables-in expression))
           (alist (mapcar #'make-renaming-pair variables)))
      (cl-sublis alist expression :key #'symbol-to-string :test #'string=))))

;;; Iterative Engine

(cl-defstruct eprolog--goal-frame
  "A pending goal in the iterative proof engine."
  goal
  cut-base)

(cl-defstruct eprolog--choice-point
  "A backtracking snapshot for the iterative proof engine."
  kind
  goal
  alternatives
  pending-goals
  bindings
  dynamic-parameters
  cut-base)

(cl-defstruct eprolog--engine
  "State for the iterative Prolog evaluator."
  goals
  bindings
  choice-points
  dynamic-parameters
  query-variables
  step-count
  finished-p
  yielded-p)

(defvar eprolog-current-engine nil
  "Current iterative engine instance while a built-in is running.")

(defvar eprolog-current-frame nil
  "Current goal frame while a built-in is running.")

(defun eprolog--goal-predicate-symbol (goal)
  "Return GOAL's predicate symbol."
  (if (consp goal) (car goal) goal))

(defun eprolog--goal-arguments (goal)
  "Return GOAL's argument list."
  (if (consp goal) (cdr goal) '()))

(defun eprolog--goal-for-unify (goal)
  "Return GOAL in clause-head shape for unification."
  (if (consp goal) goal (list goal)))

(defun eprolog--prepend-goals (goals cut-base pending-goals)
  "Prepend GOALS to PENDING-GOALS with CUT-BASE."
  (let ((frames pending-goals))
    (dolist (goal (reverse goals) frames)
      (setq frames (cons (make-eprolog--goal-frame :goal goal :cut-base cut-base)
                         frames)))))

(defun eprolog--push-choice-point (engine choice-point)
  "Push CHOICE-POINT onto ENGINE."
  (setf (eprolog--engine-choice-points engine)
        (cons choice-point (eprolog--engine-choice-points engine))))

(defun eprolog--make-engine (goals)
  "Create an iterative engine for GOALS."
  (let* ((prepared-goals (eprolog--replace-anonymous-variables goals))
         (query-variables (eprolog--variables-in goals)))
    (make-eprolog--engine
     :goals (eprolog--prepend-goals prepared-goals 0 '())
     :bindings '()
     :choice-points '()
     :dynamic-parameters '()
     :query-variables query-variables
     :step-count 0
     :finished-p nil
     :yielded-p nil)))

(defun eprolog--collect-query-bindings (engine)
  "Collect visible query bindings from ENGINE."
  (mapcar (lambda (variable)
            (cons variable
                  (eprolog--substitute-bindings
                   (eprolog--engine-bindings engine)
                   variable)))
          (eprolog--engine-query-variables engine)))

(defun eprolog--try-clauses (engine goal clauses pending-goals bindings dynamic-parameters cut-base)
  "Try CLAUSES for GOAL on ENGINE.
Return non-nil if a clause matched and updated ENGINE."
  (let ((matched nil))
    (while (and clauses (not matched))
      (let* ((current-clause (eprolog--rename-vars (car clauses)))
             (clause-head (car current-clause))
             (clause-body (cdr current-clause))
             (new-bindings (eprolog--unify (eprolog--goal-for-unify goal)
                                           clause-head
                                           bindings)))
        (if (eprolog--failure-p new-bindings)
            (setq clauses (cdr clauses))
          (let ((remaining-clauses (cdr clauses)))
            (when remaining-clauses
              (eprolog--push-choice-point
               engine
               (make-eprolog--choice-point
                :kind :clauses
                :goal goal
                :alternatives remaining-clauses
                :pending-goals pending-goals
                :bindings bindings
                :dynamic-parameters dynamic-parameters
                :cut-base cut-base)))
            (setf (eprolog--engine-bindings engine) new-bindings)
            (setf (eprolog--engine-dynamic-parameters engine) dynamic-parameters)
            (setf (eprolog--engine-goals engine)
                  (eprolog--prepend-goals clause-body cut-base pending-goals))
            (setq matched t)))))
    matched))

(defun eprolog--resume-from-choice-point (engine)
  "Resume ENGINE from its most recent choice point.
Return non-nil if execution can continue."
  (catch 'resumed
    (while (eprolog--engine-choice-points engine)
      (let* ((choice-point (pop (eprolog--engine-choice-points engine)))
             (kind (eprolog--choice-point-kind choice-point))
             (pending-goals (eprolog--choice-point-pending-goals choice-point))
             (bindings (eprolog--choice-point-bindings choice-point))
             (dynamic-parameters (eprolog--choice-point-dynamic-parameters choice-point))
             (cut-base (eprolog--choice-point-cut-base choice-point))
             (alternatives (eprolog--choice-point-alternatives choice-point)))
        (setf (eprolog--engine-bindings engine) bindings)
        (setf (eprolog--engine-dynamic-parameters engine) dynamic-parameters)
        (setf (eprolog--engine-goals engine) pending-goals)
        (pcase kind
          (:clauses
           (when (eprolog--try-clauses engine
                                       (eprolog--choice-point-goal choice-point)
                                       alternatives
                                       pending-goals
                                       bindings
                                       dynamic-parameters
                                       cut-base)
             (throw 'resumed t)))
          (:goals
           (when alternatives
             (let ((goal-sequence (car alternatives))
                   (remaining (cdr alternatives)))
               (when remaining
                 (eprolog--push-choice-point
                  engine
                  (make-eprolog--choice-point
                   :kind :goals
                   :alternatives remaining
                   :pending-goals pending-goals
                   :bindings bindings
                   :dynamic-parameters dynamic-parameters
                   :cut-base cut-base)))
               (setf (eprolog--engine-goals engine)
                     (eprolog--prepend-goals goal-sequence cut-base pending-goals))
               (throw 'resumed t)))))))
    nil))

(defun eprolog--engine-fail (engine)
  "Move ENGINE to the next backtracking alternative."
  (unless (eprolog--resume-from-choice-point engine)
    (setf (eprolog--engine-finished-p engine) t))
  engine)

(defun eprolog--engine-push-goals (engine goals cut-base)
  "Prepend GOALS to ENGINE's pending goals using CUT-BASE."
  (setf (eprolog--engine-goals engine)
        (eprolog--prepend-goals goals
                                cut-base
                                (eprolog--engine-goals engine))))

(defun eprolog--engine-unify! (engine left right)
  "Unify LEFT and RIGHT against ENGINE's bindings.
Return :ok on success and update ENGINE, otherwise return :fail."
  (let ((new-bindings (eprolog--unify left right (eprolog--engine-bindings engine))))
    (if (eprolog--failure-p new-bindings)
        :fail
      (setf (eprolog--engine-bindings engine) new-bindings)
      :ok)))

(defun eprolog--spy-before-goal (goal bindings)
  "Display spy CALL trace for GOAL with BINDINGS.
Return non-nil when the goal is being spied."
  (let* ((predicate-symbol (eprolog--goal-predicate-symbol goal))
         (spy-p (member predicate-symbol eprolog-spy-predicates))
         (show-p (and spy-p
                      (pcase eprolog-spy-state
                        ('always t)
                        ('prompt (eprolog--spy-prompt goal bindings))
                        (_ nil)))))
    (when show-p
      (eprolog--spy-message "CALL" goal bindings))
    show-p))

(defun eprolog--spy-after-goal (goal before-bindings after-bindings success-p show-p)
  "Display spy EXIT/FAIL trace for GOAL."
  (when show-p
    (if success-p
        (eprolog--spy-message "EXIT" goal after-bindings)
      (eprolog--spy-message "FAIL" goal before-bindings))))

(defun eprolog--dispatch-goal (engine frame)
  "Dispatch FRAME on ENGINE.
Return non-nil on success, nil on immediate failure."
  (let* ((goal (eprolog--goal-frame-goal frame))
         (predicate-symbol (eprolog--goal-predicate-symbol goal))
         (predicate-handler (eprolog--get-clauses predicate-symbol))
         (args (eprolog--goal-arguments goal)))
    (cond
     ((functionp predicate-handler)
      (let* ((eprolog-current-engine engine)
             (eprolog-current-frame frame)
             (eprolog-current-bindings (eprolog--engine-bindings engine))
             (eprolog-remaining-goals (mapcar #'eprolog--goal-frame-goal
                                             (eprolog--engine-goals engine)))
             (eprolog-dynamic-parameters (eprolog--engine-dynamic-parameters engine))
             (status (apply predicate-handler engine frame args)))
        (eq status :ok)))
     ((and (listp predicate-handler) predicate-handler)
      (let ((cut-base (length (eprolog--engine-choice-points engine))))
        (eprolog--try-clauses engine
                              goal
                              predicate-handler
                              (eprolog--engine-goals engine)
                              (eprolog--engine-bindings engine)
                              (eprolog--engine-dynamic-parameters engine)
                              cut-base)))
     (t nil))))

(defun eprolog--engine-next-solution (engine)
  "Return ENGINE's next solution, or nil if exhausted."
  (when (eprolog--engine-yielded-p engine)
    (setf (eprolog--engine-yielded-p engine) nil)
    (eprolog--engine-fail engine))
  (catch 'solution
    (while (not (eprolog--engine-finished-p engine))
      (if (null (eprolog--engine-goals engine))
          (progn
            (setf (eprolog--engine-yielded-p engine) t)
            (throw 'solution (eprolog--collect-query-bindings engine)))
        (let* ((frame (pop (eprolog--engine-goals engine)))
               (goal (eprolog--goal-frame-goal frame))
               (before-bindings (eprolog--engine-bindings engine))
               (show-p (eprolog--spy-before-goal goal before-bindings))
               (success-p (eprolog--dispatch-goal engine frame)))
          (setf (eprolog--engine-step-count engine)
                (1+ (eprolog--engine-step-count engine)))
          (when (and eprolog-max-steps
                     (> (eprolog--engine-step-count engine) eprolog-max-steps))
            (signal 'eprolog-step-limit-exceeded
                    (list :max-steps eprolog-max-steps
                          :goal goal
                          :step-count (eprolog--engine-step-count engine))))
          (eprolog--spy-after-goal goal
                                   before-bindings
                                   (eprolog--engine-bindings engine)
                                   success-p
                                   show-p)
          (unless success-p
            (eprolog--engine-fail engine)))))
    nil))

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
KIND is typically \='CALL\=', \='EXIT\=', or \='FAIL\='.
GOAL is the goal being traced.
BINDINGS is the current variable binding environment.

Shows the resolved goal after applying current variable bindings."
  (let ((resolved (eprolog--substitute-bindings bindings goal)))
    (eprolog--printf "\n** %s: %S" kind resolved)))

(defun eprolog--display-solution (bindings)
  "Display variable BINDINGS as a solution.
BINDINGS is an alist of variable-value pairs.

Shows \='Yes\=' for solutions with no variable bindings (pure facts),
otherwise displays each variable-value pair on separate lines."
  (if (null bindings)
      (eprolog--printf "\nYes")
    (dolist (var-val bindings)
      (eprolog--printf "\n%s = %s" (car var-val) (cdr var-val)))))

(defun eprolog--run (goals)
  "Execute GOALS as an interactive query with user continuation prompt.
GOALS is the list of goals to execute.

Displays each solution and prompts whether to continue searching for more.
Shows \='No\=' if no solutions exist.  This is the main entry point for
interactive queries initiated by `eprolog-query\='."
  (let ((solutions))
    (cl-block query-exit
      (eprolog-solve
       goals
       :success
       (lambda (solution)
         (let* ((formatted-solution
                 (with-output-to-string
                   (eprolog--display-solution solution)))
                (prompt (concat formatted-solution "\n\nContinue?")))
           (push solution solutions)
           (unless (y-or-n-p prompt)
             (cl-return-from query-exit solutions))))
       :failure
       (lambda ()
         (message "\nNo")
         (cl-return-from query-exit solutions))))))

;;; Lisp Integration Helper

(defun eprolog--eval-lisp-expressions (expressions &optional result-handler)
  "Evaluate EXPRESSIONS as Lisp code for Prolog integration.
RESULT-HANDLER processes the evaluation result if provided."
  (if (not (eprolog--ground-p expressions))
      :fail
    (let* ((lisp-expression (eprolog--substitute-bindings eprolog-current-bindings `(progn ,@expressions)))
           (evaluated-result (eval lisp-expression)))
      (if result-handler
          (funcall result-handler evaluated-result)
        :ok))))

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
  (eprolog-solve \='((parent tom _x)))
  (eprolog-solve \='((parent tom _x)) :success #\='print)
  (eprolog-solve \='((parent tom _x))
    :success (lambda (bindings) (message \"Found: %S\" bindings))
    :failure (lambda () (message \"No more solutions\")))"
  (let* ((on-success (or (plist-get args :success) (lambda (_))))
         (on-failure (or (plist-get args :failure) (lambda ())))
         (engine (eprolog--make-engine goals))
         (done nil))
    (while (not done)
      (let ((solution (eprolog--engine-next-solution engine)))
        (if (eprolog--engine-yielded-p engine)
            (funcall on-success solution)
          (setq done t))))
    (funcall on-failure)))

(defmacro eprolog-define-lisp-predicate (name args &rest body)
  "Define a predicate implemented as an Emacs Lisp function.
NAME is the predicate symbol.
ARGS is the list of formal parameters for the predicate.
BODY is the Lisp implementation that should return :ok or :fail.

Built-in predicate implementations may access `eprolog-current-engine\=' and
`eprolog-current-frame\='."
  (declare (indent defun))
  `(eprolog--set-clauses ',name (lambda (engine frame ,@args) ,@body)))

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

Similar to `eprolog-define-prolog-predicate\=' but removes existing
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
(defalias 'eprolog-define-predicate #'eprolog-define-prolog-predicate)
(defalias 'eprolog-define-predicate! #'eprolog-define-prolog-predicate!)

;;; Built-in Lisp Predicates

;; Unification predicates
(eprolog-define-lisp-predicate = (term1 term2)
  "Unification predicate: TERM1 = TERM2.
Attempts to unify TERM1 and TERM2, updating the binding environment.
Succeeds if unification is possible, fails otherwise."
  (eprolog--engine-unify! engine term1 term2))

(eprolog-define-lisp-predicate == (term1 term2)
  "Strict equality predicate: TERM1 == TERM2.
Tests if TERM1 and TERM2 are identical after variable substitution.
Does not perform unification, only tests existing equality."
  (let* ((substituted-term1 (eprolog--substitute-bindings (eprolog--engine-bindings engine) term1))
         (substituted-term2 (eprolog--substitute-bindings (eprolog--engine-bindings engine) term2)))
    (if (equal substituted-term1 substituted-term2)
        :ok
      :fail)))

;; Control predicates
(eprolog-define-lisp-predicate ! ()
  "Cut predicate: !.
Commits to the current choice, preventing backtracking to discarded alternatives."
  (let* ((cut-base (eprolog--goal-frame-cut-base frame))
         (choice-points (eprolog--engine-choice-points engine))
         (drop-count (max 0 (- (length choice-points) cut-base))))
    (setf (eprolog--engine-choice-points engine) (nthcdr drop-count choice-points))
    :ok))

(eprolog-define-lisp-predicate call (pred &rest args)
  "Meta-call predicate: call(PRED, ARGS...).
Dynamically calls PRED with additional ARGS appended.
PRED can be an atom or a compound term."
  (let* ((substituted-pred (eprolog--substitute-bindings (eprolog--engine-bindings engine) pred))
         (substituted-args (eprolog--substitute-bindings (eprolog--engine-bindings engine) args))
         (goal (pcase (cons substituted-pred substituted-args)
                 (`(,head . nil) head)
                 (`(,(pred symbolp) . ,goal-args) (cons substituted-pred goal-args))
                 (`(,(pred consp) . ,goal-args) (append substituted-pred goal-args))
                 (_ (error "call: Invalid form %S" (cons substituted-pred substituted-args)))))
         (cut-base (length (eprolog--engine-choice-points engine))))
    (eprolog--engine-push-goals engine (list goal) cut-base)
    :ok))

(eprolog-define-lisp-predicate var (term)
  "Type predicate: var(TERM).
Succeeds if TERM is an unbound variable."
  (if (eprolog--variable-p (eprolog--substitute-bindings (eprolog--engine-bindings engine) term))
      :ok
    :fail))

(eprolog-define-lisp-predicate and (&rest goals)
  (eprolog--engine-push-goals engine goals (eprolog--goal-frame-cut-base frame))
  :ok)

(eprolog-define-lisp-predicate or-2 (goal1 goal2)
  (let ((cut-base (eprolog--goal-frame-cut-base frame))
        (pending-goals (eprolog--engine-goals engine))
        (bindings (eprolog--engine-bindings engine))
        (dynamic-parameters (eprolog--engine-dynamic-parameters engine)))
    (eprolog--push-choice-point
     engine
     (make-eprolog--choice-point
      :kind :goals
      :alternatives (list (list goal2))
      :pending-goals pending-goals
      :bindings bindings
      :dynamic-parameters dynamic-parameters
      :cut-base cut-base))
    (setf (eprolog--engine-goals engine)
          (eprolog--prepend-goals (list goal1) cut-base pending-goals))
    :ok))

(eprolog-define-lisp-predicate or (&rest goals)
  (let ((branches (mapcar #'list goals)))
    (if (null branches)
        :fail
      (let ((cut-base (eprolog--goal-frame-cut-base frame))
            (pending-goals (eprolog--engine-goals engine))
            (bindings (eprolog--engine-bindings engine))
            (dynamic-parameters (eprolog--engine-dynamic-parameters engine)))
        (when (cdr branches)
          (eprolog--push-choice-point
           engine
           (make-eprolog--choice-point
            :kind :goals
            :alternatives (cdr branches)
            :pending-goals pending-goals
            :bindings bindings
            :dynamic-parameters dynamic-parameters
            :cut-base cut-base)))
        (setf (eprolog--engine-goals engine)
              (eprolog--prepend-goals (car branches) cut-base pending-goals))
        :ok))))

;; Lisp integration predicates
(eprolog-define-lisp-predicate lisp (result-variable &rest expressions)
  "Evaluate EXPRESSIONS as Lisp and unify result with RESULT-VARIABLE."
  (eprolog--eval-lisp-expressions
   expressions
   (lambda (evaluated-result)
     (let ((result-term (eprolog--substitute-bindings
                         (eprolog--engine-bindings engine)
                         result-variable)))
       (eprolog--engine-unify! engine result-term evaluated-result)))))

(eprolog-define-lisp-predicate lisp! (&rest expressions)
  "Evaluate EXPRESSIONS as Lisp for side effects, always succeeds."
  (eprolog--eval-lisp-expressions expressions))

(eprolog-define-lisp-predicate lispp (&rest expressions)
  "Evaluate EXPRESSIONS as Lisp and succeed if result is non-nil."
  (eprolog--eval-lisp-expressions
   expressions
   (lambda (evaluated-result)
     (if (not evaluated-result)
         :fail
       :ok))))

(eprolog-define-lisp-predicate store (variable-symbol value-expression)
  "Dynamic parameter predicate: store(SYMBOL, VALUE).
Stores VALUE under SYMBOL in the dynamic parameter store.
VALUE-EXPRESSION is stored directly without evaluation.

This binding persists across backtracking and is automatically
restored when execution backtracks above this store operation."
  (let* ((substituted-value (eprolog--substitute-bindings
                             (eprolog--engine-bindings engine)
                             value-expression))
         (updated-parameters (cons (cons variable-symbol substituted-value)
                                   (eprolog--engine-dynamic-parameters engine))))
    (setf (eprolog--engine-dynamic-parameters engine) updated-parameters)
    :ok))

(eprolog-define-lisp-predicate fetch (variable-symbol prolog-variable)
  "Dynamic parameter predicate: fetch(SYMBOL, VAR).
Retrieves the value associated with SYMBOL and unifies it with VAR."
  (let ((key-value (assoc variable-symbol (eprolog--engine-dynamic-parameters engine))))
    (if (null key-value)
        :fail
      (eprolog--engine-unify! engine prolog-variable (cdr key-value)))))

;;; Built-in Prolog Predicates

;; Type checking predicates
;; In SWI-Prolog, atom/1 succeeds only for symbols. Use `symbolp` to
;; mirror that behavior instead of the more permissive `atom`.
(eprolog-define-prolog-predicate! (atom _term)
  (lispp (symbolp '_term)))

(eprolog-define-prolog-predicate! (atomic _term)
  (lispp (not (consp '_term))))

(eprolog-define-prolog-predicate! (number _term)
  (lispp (numberp '_term)))

(eprolog-define-prolog-predicate! (string _term)
  (lispp (stringp '_term)))

(eprolog-define-prolog-predicate! (ground _term)
  (lisp! '_term))

(eprolog-define-prolog-predicate! (fail)
  (lisp! '_))

;; Basic logical predicates
(eprolog-define-prolog-predicate! true)
(eprolog-define-prolog-predicate! false (fail))

(eprolog-define-prolog-predicate! (not _goal)
  (call _goal) ! (fail))
(eprolog-define-prolog-predicate (not _goal))

;; Logical operators

(eprolog-define-prolog-predicate! (if _cond _then)
  (call _cond) ! (call _then))
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
Handles terminals specified via vectors, non-terminals (symbols), semantic
actions (@), cuts (!), and epsilon (empty vector []) productions.  Strings
are no longer treated as terminals."
  (if (null body)
      nil
    (let* ((element (car body))
           (rest (cdr body))
           (next-var (if rest (gensym "_") out-var)))
      (pcase element
        ;; Handle epsilon (empty production) - empty vector []
        ((and (pred vectorp) (pred (lambda (v) (= (length v) 0))))
         ;; epsilon means no consumption, so continue with same variables
         (let ((match-goal `(= ,in-var ,next-var)))
           (cons match-goal (eprolog--transform-dcg-body rest in-var out-var))))

        ;; Handle cut (!) - pass through as-is, doesn't consume input
        ('!
         (cons element (eprolog--transform-dcg-body rest in-var out-var)))

        ;; Handle variables (pass through as-is)
        ((pred eprolog--variable-p)
         (let ((match-goal `(= ,in-var (,element . ,next-var))))
           (cons match-goal (eprolog--transform-dcg-body rest next-var out-var))))

        ;; Handle vectors - sequences of terminals
        ((pred vectorp)
         (cl-loop
          with len = (length element)
          for current = in-var then next
          for tok across element
          for i from 0
          for next = (if (and (= i (1- len)) (not rest)) out-var (gensym "_"))
          collect `(= ,current (,tok . ,next)) into goals
          finally return
          (let ((rest-body (eprolog--transform-dcg-body rest current out-var)))
            (append goals rest-body))))

        ;; Cut as list: (!)
        (`(!)
         (cons '! (eprolog--transform-dcg-body rest in-var out-var)))

        ;; Semantic actions: (@ goal...)
        (`(@ . ,goals)
         (append `((= ,in-var ,next-var))
                 goals
                 (eprolog--transform-dcg-body rest in-var out-var)))

        ;; Non-terminals with arguments (lists)
        (`(,pred-name . ,pred-args)
         (let ((dcg-call `(call ,pred-name ,@pred-args ,in-var ,next-var)))
           (cons dcg-call (eprolog--transform-dcg-body rest next-var out-var))))

        ;; Non-terminals (symbols)
        ((pred symbolp)
         (let ((dcg-call `(call ,element ,in-var ,next-var)))
           (cons dcg-call (eprolog--transform-dcg-body rest next-var out-var))))

        ;; Default case
        (_ (error "Invalid DCG body element: %S" element))))))

(defun eprolog--define-grammar-impl (dcg-parts replace-p)
  "Internal implementation for DCG rule definition from DCG-PARTS.
If REPLACE-P is non-nil, replaces existing rules; otherwise adds to them."
  (let* ((head (car dcg-parts))
         (body (cdr dcg-parts))
         (head-name (if (consp head) (car head) head))
         (head-args (if (consp head) (cdr head) '()))
         (in-var (gensym "_"))
         (out-var (gensym "_"))
         (transformed-args `(,@head-args ,in-var ,out-var))
         (transformed-body (eprolog--transform-dcg-body body in-var out-var))
         (define-fn (if replace-p 'eprolog-define-prolog-predicate! 'eprolog-define-prolog-predicate)))
    `(,define-fn (,head-name ,@transformed-args) ,@transformed-body)))

(defmacro eprolog-define-grammar! (&rest dcg-parts)
  "Define a DCG rule from DCG-PARTS, replacing existing rules with the same arity."
  (eprolog--define-grammar-impl dcg-parts t))

(defmacro eprolog-define-grammar (&rest dcg-parts)
  "Define a DCG rule from DCG-PARTS, adding to existing rules with the same arity."
  (eprolog--define-grammar-impl dcg-parts nil))

;; DCG parsing predicates
(eprolog-define-prolog-predicate! (phrase _NonTerminal _List)
  (call _NonTerminal _List nil))

(eprolog-define-prolog-predicate (phrase _NonTerminal _List _Rest)
  (call _NonTerminal _List _Rest))

(provide 'eprolog)
;;; eprolog.el ends here
