;;;; eprolog.scm --- Native Prolog engine implementation in Scheme

;; Copyright (C) 2025 Masaya Taniguchi

;; Author: Masaya Taniguchi
;; Version: 0.2.0
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

;;;; Commentary:

;; ε-prolog (eprolog) is a complete Prolog engine implementation written
;; in pure Scheme.  It provides a fully functional Prolog system
;; with traditional Prolog programming capabilities and seamless Scheme
;; interoperability.
;;
;; Features:
;; - Complete unification algorithm with occurs check
;; - Backtracking and choice points with proper cut (!) semantics
;; - Clause database management for facts and rules
;; - Interactive query execution with solution enumeration
;; - Built-in predicates for type checking, control, and list operations
;; - Definite Clause Grammar (DCG) support
;; - Spy/debugging functionality for tracing execution
;; - Direct Scheme integration through special predicates
;;
;; Quick Start:
;;
;;   ;; Define facts and rules
;;   (eprolog-define-prolog-predicate 'parent '(tom bob))
;;   (eprolog-define-prolog-predicate 'parent '(bob ann))
;;
;;   (eprolog-define-prolog-predicate 'grandparent '(_x _z)
;;     '(parent _x _y)
;;     '(parent _y _z))
;;
;;   ;; Query the database
;;   (eprolog-query '(grandparent tom _x))

;;;; Code:

;;; Core Data Types & Configuration

;; Simple constructors instead of SRFI-9 records for better compatibility
(define (make-eprolog--failure) 'failure)
(define (eprolog--failure? x) (eq? x 'failure))

(define (make-eprolog--success bindings continuation)
  (vector 'success bindings continuation))
(define (eprolog--success? x) 
  (and (vector? x) (eq? (vector-ref x 0) 'success)))
(define (eprolog--success-bindings x) (vector-ref x 1))
(define (eprolog--success-continuation x) (vector-ref x 2))

(define (make-eprolog-cut-exception tag value)
  (vector 'cut-exception tag value))
(define (eprolog-cut-exception? x)
  (and (vector? x) (eq? (vector-ref x 0) 'cut-exception)))
(define (exception-tag x) (vector-ref x 1))
(define (exception-value x) (vector-ref x 2))

;; Public configuration variables

(define *eprolog-clause-database* '())
(define *eprolog-spy-predicates* '())
(define *eprolog-spy-state* 'prompt)  ; 'prompt, 'always, or 'disabled
(define *eprolog-occurs-check* #t)

;; Internal variables

(define *eprolog-current-bindings* '())
(define *eprolog-remaining-goals* '())
(define *eprolog-dynamic-parameters* '())

;; Gensym implementation for Scheme
(define *gensym-counter* 0)

(define (gensym prefix)
  (set! *gensym-counter* (+ *gensym-counter* 1))
  (string->symbol
   (string-append prefix (number->string *gensym-counter*))))

;;; Helper functions

(define (proper-list? x)
  "Check if X is a proper list (not a dotted pair)."
  (or (null? x)
      (and (pair? x)
           (proper-list? (cdr x)))))

(define (filter pred lst)
  "Filter list by predicate."
  (cond
   ((null? lst) '())
   ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
   (else (filter pred (cdr lst)))))

(define (remove-if pred lst)
  "Remove elements matching predicate."
  (filter (lambda (x) (not (pred x))) lst))

(define (remove-if-not pred lst)
  "Keep only elements matching predicate."
  (filter pred lst))

(define (member-equal x lst)
  "Check if X is member of LST using equal?"
  (cond
   ((null? lst) #f)
   ((equal? x (car lst)) #t)
   (else (member-equal x (cdr lst)))))

(define (assoc-equal key alist)
  "Association list lookup using equal?"
  (cond
   ((null? alist) #f)
   ((equal? key (caar alist)) (car alist))
   (else (assoc-equal key (cdr alist)))))

(define (remove-duplicates lst)
  "Remove duplicate elements from list, keeping last occurrence."
  (if (null? lst)
      '()
      (let ((rest (remove-duplicates (cdr lst))))
        (if (member-equal (car lst) rest)
            rest
            (cons (car lst) rest)))))

(define (reduce fn lst init)
  "Reduce/fold function."
  (if (null? lst)
      init
      (reduce fn (cdr lst) (fn (car lst) init))))

(define (sublis alist tree)
  "Substitute according to association list."
  (cond
   ((assoc tree alist) => cdr)
   ((pair? tree)
    (cons (sublis alist (car tree))
          (sublis alist (cdr tree))))
   (else tree)))

;;; Variable & Term Operations

(define (eprolog--variable? term)
  "Test if TERM is a Prolog variable."
  (and (symbol? term)
       (let ((str (symbol->string term)))
         (and (> (string-length str) 0)
              (char=? (string-ref str 0) #\_)))))

(define (eprolog--named-variable? term)
  "Test if TERM is a named Prolog variable (not anonymous)."
  (and (eprolog--variable? term)
       (not (eq? term '_))))

(define (eprolog--lookup-variable variable bindings)
  "Look up VARIABLE's value in the association list BINDINGS."
  (let ((binding (assoc variable bindings)))
    (if binding (cdr binding) #f)))

(define (eprolog--substitute-bindings bindings expression . visited)
  "Apply variable BINDINGS to EXPRESSION."
  (let ((visited (if (null? visited) '() (car visited))))
    (cond
     ((eprolog--failure? bindings) (make-eprolog--failure))
     ((null? bindings) expression)
     (else
      (cond
       ((pair? expression)
        (cons (eprolog--substitute-bindings bindings (car expression) visited)
              (eprolog--substitute-bindings bindings (cdr expression) visited)))
       ((and (eprolog--variable? expression)
             (member expression visited))
        expression)
       ((and (eprolog--variable? expression)
             (assoc expression bindings))
        (let ((value (eprolog--lookup-variable expression bindings)))
          (eprolog--substitute-bindings bindings value (cons expression visited))))
       (else expression))))))

(define (eprolog--variables-in expression)
  "Extract all named variables from EXPRESSION."
  (letrec ((collect-all-variables
            (lambda (tree)
              (if (not (pair? tree))
                  (list tree)
                  (append (collect-all-variables (car tree))
                          (collect-all-variables (cdr tree)))))))
    (let* ((all-atoms (collect-all-variables expression))
           (variables-only (filter eprolog--named-variable? all-atoms)))
      (remove-duplicates variables-only))))

(define (eprolog--replace-anonymous-variables expression)
  "Replace all anonymous '_' variables with unique generated symbols."
  (cond
   ((eq? expression '_) (gensym "_"))
   ((not (pair? expression)) expression)
   (else
    (cons (eprolog--replace-anonymous-variables (car expression))
          (eprolog--replace-anonymous-variables (cdr expression))))))

(define (eprolog--ground? term)
  "Check if TERM is fully ground (contains no unbound variables)."
  (let ((resolved-term (eprolog--substitute-bindings *eprolog-current-bindings* term)))
    (cond
     ((eprolog--variable? resolved-term) #f)
     ((pair? resolved-term)
      (and (eprolog--ground? (car resolved-term))
           (eprolog--ground? (cdr resolved-term))))
     (else #t))))

;;; Unification System

(define (eprolog--occurs-check? variable expression bindings)
  "Check if VARIABLE occurs within EXPRESSION."
  (cond
   ((and (symbol? expression) (eq? variable expression)) #t)
   ((and (eprolog--variable? expression) (assoc expression bindings))
    (let ((value (eprolog--lookup-variable expression bindings)))
      (eprolog--occurs-check? variable value bindings)))
   ((pair? expression)
    (or (eprolog--occurs-check? variable (car expression) bindings)
        (eprolog--occurs-check? variable (cdr expression) bindings)))
   (else #f)))

(define (eprolog--unify-var variable value bindings)
  "Unify VARIABLE with VALUE given current BINDINGS."
  (let ((var-binding (assoc variable bindings)))
    (cond
     (var-binding
      (eprolog--unify (cdr var-binding) value bindings))
     ((and (eprolog--variable? value) (assoc value bindings))
      (let ((bound-term (eprolog--lookup-variable value bindings)))
        (eprolog--unify variable bound-term bindings)))
     ((and *eprolog-occurs-check* (eprolog--occurs-check? variable value bindings))
      (make-eprolog--failure))
     (else (cons (cons variable value) bindings)))))

(define (eprolog--unify term1 term2 bindings)
  "Unify TERM1 and TERM2 given current BINDINGS."
  (cond
   ((eprolog--failure? bindings) (make-eprolog--failure))
   ((equal? term1 term2) bindings)
   ((eprolog--variable? term1) (eprolog--unify-var term1 term2 bindings))
   ((eprolog--variable? term2) (eprolog--unify-var term2 term1 bindings))
   ((and (pair? term1) (pair? term2))
    (let ((car-bindings (eprolog--unify (car term1) (car term2) bindings)))
      (eprolog--unify (cdr term1) (cdr term2) car-bindings)))
   (else (make-eprolog--failure))))

;;; Clause Database Management

(define (eprolog--get-clauses predicate-symbol)
  "Retrieve all clauses for PREDICATE-SYMBOL."
  (let ((entry (assoc predicate-symbol *eprolog-clause-database*)))
    (if entry (cdr entry) '())))

(define (eprolog--set-clauses predicate-symbol clauses)
  "Set the CLAUSES for PREDICATE-SYMBOL."
  (let* ((cleaned-db (remove-if (lambda (x) (eq? (car x) predicate-symbol))
                                 *eprolog-clause-database*))
         (new-db (cons (cons predicate-symbol clauses) cleaned-db)))
    (set! *eprolog-clause-database* new-db)
    (cdr (car new-db))))

(define (eprolog--add-clause clause)
  "Add CLAUSE to the current clause database."
  (let* ((predicate-symbol (caar clause))
         (current-clauses (eprolog--get-clauses predicate-symbol))
         (new-clauses (append current-clauses (list clause))))
    (eprolog--set-clauses predicate-symbol new-clauses)))

(define (eprolog--remove-clauses-with-arity! predicate-symbol arity)
  "Remove all clauses for PREDICATE-SYMBOL with specified ARITY."
  (letrec ((has-different-arity?
            (lambda (clause)
              (not (= (length (cdar clause)) arity)))))
    (let* ((current-clauses (eprolog--get-clauses predicate-symbol))
           (new-clauses (remove-if-not has-different-arity? current-clauses)))
      (eprolog--set-clauses predicate-symbol new-clauses))))

(define (eprolog--rename-vars expression)
  "Create a copy of EXPRESSION with all variables renamed to fresh symbols."
  (letrec ((make-renaming-pair
            (lambda (variable)
              (let ((var-string (symbol->string variable)))
                (cons variable (gensym var-string))))))
    (let* ((variables (eprolog--variables-in expression))
           (alist (map make-renaming-pair variables)))
      (sublis alist expression))))

;;; Cut & Choice Point Handling

(define (eprolog--wrap-success-with-cut-handler result tag)
  "Wrap RESULT so that any future cut with TAG is caught and handled."
  (if (eprolog--success? result)
      (let ((bindings (eprolog--success-bindings result))
            (cont (eprolog--success-continuation result)))
        (make-eprolog--success
         bindings
         (lambda ()
           (call/cc
            (lambda (escape)
              (with-exception-handler
               (lambda (err)
                 (if (and (eprolog-cut-exception? err)
                          (eq? tag (exception-tag err)))
                     (escape (eprolog--wrap-success-with-cut-handler
                              (exception-value err) tag))
                     (raise err)))
               (lambda ()
                 (let ((next (cont)))
                   (eprolog--wrap-success-with-cut-handler next tag)))))))))
      result))

(define (eprolog--call-with-current-choice-point proc)
  "Execute PROC with a fresh choice point tag for cut handling."
  (let ((tag (gensym "CHOICE-POINT-")))
    (call/cc
     (lambda (escape)
       (with-exception-handler
        (lambda (err)
          (if (and (eprolog-cut-exception? err)
                   (eq? tag (exception-tag err)))
              (escape (eprolog--wrap-success-with-cut-handler
                       (exception-value err) tag))
              (raise err)))
        (lambda ()
          (let ((result (proc tag)))
            (eprolog--wrap-success-with-cut-handler result tag))))))))

(define (eprolog--insert-choice-point clause choice-point)
  "Insert CHOICE-POINT tag into cut operators (!) within CLAUSE."
  (letrec ((insert-cut-term
            (lambda (term)
              (cond
               ((eq? term '!)
                (list '! choice-point))
               ((equal? term '(!))
                (list '! choice-point))
               ((and (pair? term)
                     (eq? (car term) '!)
                     (not (null? (cdr term))))
                term)  ; Already has choice-point
               ((proper-list? term)
                (map insert-cut-term term))
               ((pair? term)
                (cons (insert-cut-term (car term))
                      (insert-cut-term (cdr term))))
               (else term)))))
    (map insert-cut-term clause)))

(define (eprolog--merge-continuations continuation-a continuation-b)
  "Merge two backtracking continuations into a single continuation."
  (lambda ()
    (let ((result-a (continuation-a)))
      (if (or (not result-a) (eprolog--failure? result-a))
          (continuation-b)
          (let* ((bindings (eprolog--success-bindings result-a))
                 (result-continuation (eprolog--success-continuation result-a))
                 (new-continuation (eprolog--merge-continuations
                                    result-continuation continuation-b)))
            (make-eprolog--success bindings new-continuation))))))

;;; Proof Engine Core

(define (eprolog--prove-goal-sequence goals bindings)
  "Prove a sequence of GOALS with the given BINDINGS."
  (cond
   ((eprolog--failure? bindings) (make-eprolog--failure))
   ((null? goals)
    (let ((terminal-cont (lambda () (make-eprolog--failure))))
      (make-eprolog--success bindings terminal-cont)))
   (else (eprolog--prove-goal goals bindings))))

(define (eprolog--prove-goal goals bindings)
  "Prove the first goal in GOALS with the given BINDINGS."
  (let* ((goal (car goals))
         (remaining-goals (cdr goals))
         (predicate-symbol (if (pair? goal) (car goal) goal))
         (predicate-handler (eprolog--get-clauses predicate-symbol))
         (args (if (pair? goal) (cdr goal) '())))
    (eprolog--with-spy
     goal bindings
     (lambda ()
       (cond
        ((procedure? predicate-handler)
         (let* ((substituted-args (eprolog--substitute-bindings bindings args))
                (old-remaining-goals *eprolog-remaining-goals*)
                (old-current-bindings *eprolog-current-bindings*))
           (set! *eprolog-remaining-goals* remaining-goals)
           (set! *eprolog-current-bindings* bindings)
           (let ((result (apply predicate-handler substituted-args)))
             (set! *eprolog-remaining-goals* old-remaining-goals)
             (set! *eprolog-current-bindings* old-current-bindings)
             result)))
        (else (eprolog--search-matching-clauses goals bindings predicate-handler)))))))

(define (eprolog--apply-clause-to-goal goals bindings clause)
  "Apply CLAUSE to prove the first goal in GOALS with BINDINGS."
  (let* ((goal (car goals))
         (remaining-goals (cdr goals))
         (goal-for-unify (if (pair? goal) goal (list goal)))
         (renamed-clause (eprolog--rename-vars clause))
         (clause-head (car renamed-clause))
         (clause-body (cdr renamed-clause))
         (new-bindings (eprolog--unify goal-for-unify clause-head bindings)))
    (if (eprolog--failure? new-bindings)
        (make-eprolog--failure)
        (eprolog--prove-goal-sequence (append clause-body remaining-goals) new-bindings))))

(define (eprolog--search-matching-clauses goals bindings all-clauses)
  "Search through ALL-CLAUSES to find matches for the first goal in GOALS."
  (eprolog--call-with-current-choice-point
   (lambda (choice-point)
     (letrec ((try-one-by-one
               (lambda (clauses-to-try)
                 (if (null? clauses-to-try)
                     (make-eprolog--failure)
                     (let* ((current-clause (eprolog--insert-choice-point
                                             (car clauses-to-try) choice-point))
                            (remaining-clauses (cdr clauses-to-try))
                            (try-next-clause (lambda ()
                                               (try-one-by-one remaining-clauses)))
                            (result (eprolog--apply-clause-to-goal
                                     goals bindings current-clause)))
                       (if (eprolog--failure? result)
                           (try-next-clause)
                           (let* ((result-bindings (eprolog--success-bindings result))
                                  (result-continuation (eprolog--success-continuation result))
                                  (new-continuation (eprolog--merge-continuations
                                                     result-continuation try-next-clause)))
                             (make-eprolog--success result-bindings new-continuation))))))))
       (try-one-by-one all-clauses)))))

;;; Debugging & Output

(define (eprolog--printf format-string . args)
  "Print formatted output."
  (apply format #t format-string args))

(define (eprolog--spy-prompt goal bindings)
  "Display an interactive spy prompt for GOAL with current BINDINGS."
  (let ((resolved (eprolog--substitute-bindings bindings goal)))
    (format #t "SPY: ~S (press Enter to continue)~%" resolved)
    (force-output)
    (read-line)
    #t))

(define (eprolog--spy-message kind goal bindings)
  "Display a spy trace message of KIND for GOAL with BINDINGS."
  (let ((resolved (eprolog--substitute-bindings bindings goal)))
    (eprolog--printf "~%** ~A: ~S" kind resolved)))

(define (eprolog--with-spy goal bindings thunk)
  "Execute THUNK with spy tracing for GOAL using BINDINGS."
  (let* ((predicate-symbol (if (pair? goal) (car goal) goal))
         (spy? (member predicate-symbol *eprolog-spy-predicates*))
         (show? (and spy?
                     (case *eprolog-spy-state*
                       ((always) #t)
                       ((prompt) (eprolog--spy-prompt goal bindings))
                       (else #f))))
         (result #f))
    (when show?
      (eprolog--spy-message "CALL" goal bindings))
    (set! result (thunk))
    (when show?
      (if (or (not result) (eprolog--failure? result))
          (eprolog--spy-message "FAIL" goal bindings)
          (eprolog--spy-message "EXIT" goal (eprolog--success-bindings result))))
    result))

(define (eprolog--display-solution bindings)
  "Display variable BINDINGS as a solution."
  (if (null? bindings)
      (eprolog--printf "~%Yes")
      (for-each
       (lambda (var-val)
         (eprolog--printf "~%~A = ~A" (car var-val) (cdr var-val)))
       bindings)))

(define (eprolog--y-or-n? prompt)
  "Simple yes-or-no prompt."
  (display prompt)
  (display " (y or n) ")
  (force-output)
  (let ((response (read)))
    (eq? response 'y)))

(define (eprolog--run goals)
  "Execute GOALS as an interactive query with user continuation prompt."
  (let ((solutions '()))
    (call/cc
     (lambda (query-exit)
       (eprolog-solve
        goals
        'success
        (lambda (solution)
          (let* ((formatted-solution
                  (with-output-to-string
                    (lambda ()
                      (eprolog--display-solution solution))))
                 (prompt (string-append formatted-solution "\n\nContinue?")))
            (set! solutions (cons solution solutions))
            (unless (eprolog--y-or-n? prompt)
              (query-exit solutions))))
        'failure
        (lambda ()
          (display "\nNo\n")
          (query-exit solutions)))))))

;;; Lisp Integration Helper

(define (eprolog--eval-lisp-expressions expressions . result-handler)
  "Evaluate EXPRESSIONS as Scheme code for Prolog integration."
  (let ((result-handler (if (null? result-handler) #f (car result-handler))))
    (if (not (eprolog--ground? expressions))
        (make-eprolog--failure)
        (let* ((lisp-expression (eprolog--substitute-bindings
                                 *eprolog-current-bindings*
                                 (cons 'begin expressions)))
               (evaluated-result (eval lisp-expression
                                        (interaction-environment))))
          (if result-handler
              (result-handler evaluated-result)
              (eprolog--prove-goal-sequence *eprolog-remaining-goals*
                                             *eprolog-current-bindings*))))))

;;; Public API

(define (eprolog-solve goals . args)
  "Solve GOALS and call callbacks for each solution."
  (let* ((on-success (let ((s (memq 'success args)))
                       (if (and s (not (null? (cdr s))))
                           (cadr s)
                           (lambda (x) #f))))
         (on-failure (let ((f (memq 'failure args)))
                       (if (and f (not (null? (cdr f))))
                           (cadr f)
                           (lambda () #f)))))
    (letrec ((initial-continuation
              (lambda ()
                (eprolog--call-with-current-choice-point
                 (lambda (choice-point)
                   (let* ((prepared-goals (eprolog--replace-anonymous-variables goals))
                          (cut-goals (eprolog--insert-choice-point prepared-goals choice-point)))
                     (eprolog--prove-goal-sequence cut-goals '()))))))
             (retrieve-success-bindings
              (lambda (result)
                (let* ((bindings (eprolog--success-bindings result))
                       (query-variables (eprolog--variables-in goals))
                       (make-binding-pair (lambda (v)
                                             (cons v (eprolog--substitute-bindings bindings v)))))
                  (map make-binding-pair query-variables))))
             (execute-success-continuation
              (lambda (result)
                ((eprolog--success-continuation result)))))
      (let loop ((result (initial-continuation)))
        (if (eprolog--success? result)
            (begin
              (on-success (retrieve-success-bindings result))
              (loop (execute-success-continuation result)))
            (on-failure))))))

(define-syntax eprolog-define-lisp-predicate
  (syntax-rules ()
    ((_ name args body ...)
     (eprolog--set-clauses (quote name) (lambda args body ...)))))

(define (eprolog-define-prolog-predicate head . body)
  "Define a Prolog clause (fact or rule) and add it to the clause database."
  (let* ((head-list (if (pair? head) head (list head)))
         (clause (cons head-list body)))
    (eprolog--add-clause (eprolog--replace-anonymous-variables clause))))

(define (eprolog-define-prolog-predicate! head . body)
  "Define a Prolog clause, replacing existing clauses with the same arity."
  (let* ((head-list (if (pair? head) head (list head)))
         (name (car head-list))
         (args (cdr head-list))
         (arity (length args))
         (clause (cons head-list body)))
    (eprolog--remove-clauses-with-arity! name arity)
    (eprolog--add-clause (eprolog--replace-anonymous-variables clause))))

(define-syntax eprolog-query
  (syntax-rules ()
    ((_ goals ...)
     (eprolog--run (eprolog--replace-anonymous-variables '(goals ...))))))

;; Aliases for convenience
(define eprolog-define-predicate eprolog-define-prolog-predicate)
(define eprolog-define-predicate! eprolog-define-prolog-predicate!)

;;; Built-in Lisp Predicates

;; Unification predicates
(eprolog-define-lisp-predicate = (term1 term2)
  (let ((new-bindings (eprolog--unify term1 term2 *eprolog-current-bindings*)))
    (if (eprolog--failure? new-bindings)
        (make-eprolog--failure)
        (eprolog--prove-goal-sequence *eprolog-remaining-goals* new-bindings))))

(eprolog-define-lisp-predicate == (term1 term2)
  (let* ((substituted-term1 (eprolog--substitute-bindings *eprolog-current-bindings* term1))
         (substituted-term2 (eprolog--substitute-bindings *eprolog-current-bindings* term2)))
    (if (equal? substituted-term1 substituted-term2)
        (eprolog--prove-goal-sequence *eprolog-remaining-goals* *eprolog-current-bindings*)
        (make-eprolog--failure))))

;; Control predicates
(eprolog-define-lisp-predicate ! (choice-point)
  (let ((continuation-result (eprolog--prove-goal-sequence *eprolog-remaining-goals*
                                                            *eprolog-current-bindings*)))
    (raise (make-eprolog-cut-exception choice-point continuation-result))))

(eprolog-define-lisp-predicate call (pred . args)
  (eprolog--call-with-current-choice-point
   (lambda (choice-point)
     (let* ((substituted-pred (eprolog--substitute-bindings *eprolog-current-bindings* pred))
            (substituted-args (eprolog--substitute-bindings *eprolog-current-bindings* args))
            (goal (cond
                   ((and (null? substituted-args) substituted-pred) substituted-pred)
                   ((symbol? substituted-pred) (cons substituted-pred substituted-args))
                   ((pair? substituted-pred) (append substituted-pred substituted-args))
                   (else (error "call: Invalid form" (cons substituted-pred substituted-args)))))
            (cut-goals (eprolog--insert-choice-point (list goal) choice-point))
            (next-goals (append cut-goals *eprolog-remaining-goals*)))
       (eprolog--prove-goal-sequence next-goals *eprolog-current-bindings*)))))

(eprolog-define-lisp-predicate var (term)
  (if (eprolog--variable? (eprolog--substitute-bindings *eprolog-current-bindings* term))
      (eprolog--prove-goal-sequence *eprolog-remaining-goals* *eprolog-current-bindings*)
      (make-eprolog--failure)))

(eprolog-define-lisp-predicate and goals
  (let* ((next-goals (append goals *eprolog-remaining-goals*)))
    (eprolog--prove-goal-sequence next-goals *eprolog-current-bindings*)))

(eprolog-define-lisp-predicate or-2 (goal1 goal2)
  (let* ((original-bindings *eprolog-current-bindings*)
         (remaining-goals *eprolog-remaining-goals*)
         (goals-1 (cons (list 'call goal1) remaining-goals))
         (result-1 (eprolog--prove-goal-sequence goals-1 original-bindings))
         (goals-2 (cons (list 'call goal2) remaining-goals))
         (try-goal-2 (lambda () (eprolog--prove-goal-sequence goals-2 original-bindings))))
    (if (eprolog--failure? result-1)
        (eprolog--prove-goal-sequence goals-2 original-bindings)
        (let* ((success-bindings-1 (eprolog--success-bindings result-1))
               (success-continuation-1 (eprolog--success-continuation result-1))
               (new-continuation (eprolog--merge-continuations success-continuation-1 try-goal-2)))
          (make-eprolog--success success-bindings-1 new-continuation)))))

(eprolog-define-lisp-predicate or goals
  (let* ((or-2-goal (reduce (lambda (expr acc) (list 'or-2 expr acc))
                             goals 'false))
         (next-goals (cons or-2-goal *eprolog-remaining-goals*)))
    (eprolog--prove-goal-sequence next-goals *eprolog-current-bindings*)))

;; Scheme integration predicates
(eprolog-define-lisp-predicate lisp (result-variable . expressions)
  (eprolog--eval-lisp-expressions
   expressions
   (lambda (evaluated-result)
     (let* ((result-term (eprolog--substitute-bindings *eprolog-current-bindings* result-variable))
            (new-bindings (eprolog--unify result-term evaluated-result *eprolog-current-bindings*)))
       (if (eprolog--failure? new-bindings)
           (make-eprolog--failure)
           (eprolog--prove-goal-sequence *eprolog-remaining-goals* new-bindings))))))

(eprolog-define-lisp-predicate lisp! expressions
  (eprolog--eval-lisp-expressions expressions))

(eprolog-define-lisp-predicate lispp expressions
  (eprolog--eval-lisp-expressions
   expressions
   (lambda (evaluated-result)
     (if (not evaluated-result)
         (make-eprolog--failure)
         (eprolog--prove-goal-sequence *eprolog-remaining-goals* *eprolog-current-bindings*)))))

;; Dynamic parameter predicates
(define (eprolog--with-dynamic-parameter-restoration new-parameters result)
  "Helper function to wrap RESULT with dynamic parameter restoration."
  (if (eprolog--failure? result)
      (make-eprolog--failure)
      (make-eprolog--success
       (eprolog--success-bindings result)
       (lambda ()
         (let ((*eprolog-dynamic-parameters* new-parameters))
           (eprolog--with-dynamic-parameter-restoration
            new-parameters
            ((eprolog--success-continuation result))))))))

(eprolog-define-lisp-predicate store (variable-symbol value-expression)
  (let* ((substituted-value (eprolog--substitute-bindings
                             *eprolog-current-bindings*
                             value-expression))
         (old-parameters *eprolog-dynamic-parameters*)
         (updated-parameters (cons (cons variable-symbol substituted-value)
                                   *eprolog-dynamic-parameters*)))
    (set! *eprolog-dynamic-parameters* updated-parameters)
    (let ((result (eprolog--prove-goal-sequence *eprolog-remaining-goals*
                                                *eprolog-current-bindings*)))
      (set! *eprolog-dynamic-parameters* old-parameters)
      result)))

(eprolog-define-lisp-predicate fetch (variable-symbol prolog-variable)
  (let ((key-value (assoc variable-symbol *eprolog-dynamic-parameters*)))
    (if (not key-value)
        (make-eprolog--failure)
        (let ((new-bindings (eprolog--unify prolog-variable (cdr key-value)
                                             *eprolog-current-bindings*)))
          (if (eprolog--failure? new-bindings)
              (make-eprolog--failure)
              (eprolog--prove-goal-sequence *eprolog-remaining-goals* new-bindings))))))

;;; Built-in Prolog Predicates

;; Type checking predicates
(eprolog-define-prolog-predicate! '(atom _term)
  '(lispp (symbol? '_term)))

(eprolog-define-prolog-predicate! '(atomic _term)
  '(lispp (not (pair? '_term))))

(eprolog-define-prolog-predicate! '(number _term)
  '(lispp (number? '_term)))

(eprolog-define-prolog-predicate! '(string _term)
  '(lispp (string? '_term)))

(eprolog-define-prolog-predicate! '(ground _term)
  '(lisp! '_term))

(eprolog-define-prolog-predicate! '(fail)
  '(lisp! '_))

;; Basic logical predicates
(eprolog-define-prolog-predicate! 'true)
(eprolog-define-prolog-predicate! 'false '(fail))

(eprolog-define-prolog-predicate! '(not _goal)
  '(call _goal) '! '(fail))
(eprolog-define-prolog-predicate '(not _goal))

;; Logical operators
(eprolog-define-prolog-predicate! '(if _cond _then)
  '(call _cond) '(call _then))
(eprolog-define-prolog-predicate! '(if _cond _then _else)
  '(call _cond) '! '(call _then))
(eprolog-define-prolog-predicate '(if _cond _then _else)
  '(call _else))

;; Arithmetic
(eprolog-define-prolog-predicate! '(is _result _expression)
  '(lisp _result _expression))

;; Control flow
(eprolog-define-prolog-predicate! 'repeat)
(eprolog-define-prolog-predicate 'repeat '(repeat))

;; List operations
(eprolog-define-prolog-predicate! '(member _item (_item . _)))
(eprolog-define-prolog-predicate '(member _item (_ . _rest)) '(member _item _rest))

(eprolog-define-prolog-predicate! '(append () _list _list))
(eprolog-define-prolog-predicate '(append (_head . _tail) _list (_head . _result))
  '(append _tail _list _result))

(eprolog-define-prolog-predicate! '(append () ()))
(eprolog-define-prolog-predicate '(append (_head . _tail) _result)
  '(append _tail _tail-result)
  '(append _head _tail-result _result))

;; Higher-order predicates
(eprolog-define-prolog-predicate! '(maplist _goal ()))
(eprolog-define-prolog-predicate '(maplist _goal (_head . _tail))
  '(call _goal _head)
  '(maplist _goal _tail))

(eprolog-define-prolog-predicate! '(maplist _goal () ()))
(eprolog-define-prolog-predicate '(maplist _goal (_head1 . _tail1) (_head2 . _tail2))
  '(call _goal _head1 _head2)
  '(maplist _goal _tail1 _tail2))

(eprolog-define-prolog-predicate! '(maplist _goal () () ()))
(eprolog-define-prolog-predicate '(maplist _goal (_head1 . _tail1) (_head2 . _tail2) (_head3 . _tail3))
  '(call _goal _head1 _head2 _head3)
  '(maplist _goal _tail1 _tail2 _tail3))

(eprolog-define-prolog-predicate! '(maplist _goal () () () ()))
(eprolog-define-prolog-predicate '(maplist _goal (_head1 . _tail1) (_head2 . _tail2) (_head3 . _tail3) (_head4 . _tail4))
  '(call _goal _head1 _head2 _head3 _head4)
  '(maplist _goal _tail1 _tail2 _tail3 _tail4))

;;; DCG (Definite Clause Grammar) Support

(define (eprolog--transform-dcg-body body in-var out-var)
  "Transform a DCG BODY into Prolog goals with difference lists."
  (if (null? body)
      '()
      (let* ((element (car body))
             (rest (cdr body))
             (next-var (if (not (null? rest)) (gensym "_") out-var)))
        (cond
         ;; Handle epsilon (empty production) - empty vector #()
         ((and (vector? element) (= (vector-length element) 0))
          (let ((match-goal (list '= in-var next-var)))
            (cons match-goal (eprolog--transform-dcg-body rest in-var out-var))))
         
         ;; Handle cut (!) - pass through as-is
         ((eq? element '!)
          (cons element (eprolog--transform-dcg-body rest in-var out-var)))
         
         ;; Handle variables
         ((eprolog--variable? element)
          (let ((match-goal (list '= in-var (cons element next-var))))
            (cons match-goal (eprolog--transform-dcg-body rest next-var out-var))))
         
         ;; Handle vectors - sequences of terminals
         ((vector? element)
          (let loop ((i 0)
                     (current in-var)
                     (goals '()))
            (if (< i (vector-length element))
                (let* ((tok (vector-ref element i))
                       (next (if (and (= i (- (vector-length element) 1))
                                      (null? rest))
                                 out-var
                                 (gensym "_")))
                       (goal (list '= current (cons tok next))))
                  (loop (+ i 1) next (append goals (list goal))))
                (append goals (eprolog--transform-dcg-body rest current out-var)))))
         
         ;; Cut as list: (!)
         ((equal? element '(!))
          (cons '! (eprolog--transform-dcg-body rest in-var out-var)))
         
         ;; Semantic actions: (@ goal...)
         ((and (pair? element) (eq? (car element) '@))
          (append (list (list '= in-var next-var))
                  (cdr element)
                  (eprolog--transform-dcg-body rest in-var out-var)))
         
         ;; Non-terminals with arguments
         ((and (pair? element) (not (eq? (car element) '@)))
          (let* ((pred-name (car element))
                 (pred-args (cdr element))
                 (dcg-call (append (list 'call pred-name) pred-args (list in-var next-var))))
            (cons dcg-call (eprolog--transform-dcg-body rest next-var out-var))))
         
         ;; Non-terminals (symbols)
         ((symbol? element)
          (let ((dcg-call (list 'call element in-var next-var)))
            (cons dcg-call (eprolog--transform-dcg-body rest next-var out-var))))
         
         ;; Default case
         (else (error "Invalid DCG body element:" element))))))

(define (eprolog--define-grammar-impl dcg-parts replace?)
  "Internal implementation for DCG rule definition."
  (let* ((head (car dcg-parts))
         (body (cdr dcg-parts))
         (head-name (if (pair? head) (car head) head))
         (head-args (if (pair? head) (cdr head) '()))
         (in-var (gensym "_"))
         (out-var (gensym "_"))
         (transformed-args (append head-args (list in-var out-var)))
         (transformed-body (eprolog--transform-dcg-body body in-var out-var)))
    (if replace?
        (apply eprolog-define-prolog-predicate! (cons (cons head-name transformed-args)
                                                       transformed-body))
        (apply eprolog-define-prolog-predicate (cons (cons head-name transformed-args)
                                                      transformed-body)))))

(define (eprolog-define-grammar head . body)
  "Define a DCG rule, adding to existing rules with the same arity."
  (eprolog--define-grammar-impl (cons head body) #f))

(define (eprolog-define-grammar! head . body)
  "Define a DCG rule, replacing existing rules with the same arity."
  (eprolog--define-grammar-impl (cons head body) #t))

;; DCG parsing predicates
(eprolog-define-prolog-predicate! '(phrase _NonTerminal _List)
  '(call _NonTerminal _List ()))

(eprolog-define-prolog-predicate '(phrase _NonTerminal _List _Rest)
  '(call _NonTerminal _List _Rest))

;;; End of eprolog.scm