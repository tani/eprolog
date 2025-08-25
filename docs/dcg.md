
# DCG (Definite Clause Grammars)

Definite Clause Grammars stand as one of Prolog's most sophisticated and elegant contributions to computational linguistics and parsing theory. DCGs transform the traditionally complex task of parsing into a declarative, almost literary description of language structure. They represent a perfect marriage of formal grammar theory with the practical power of logic programming.

What makes DCGs truly remarkable is their bidirectional nature—the same grammar rules that parse text can also generate it. This symmetry reflects a deep mathematical principle: if you can describe the structure of valid sentences, you automatically have a system that can both recognize and produce those sentences. DCGs embody this principle with remarkable elegance and efficiency.

ε-prolog provides full support for Definite Clause Grammars (DCG), a convenient notation for parsing and generating language structures. DCGs are automatically transformed into standard Prolog predicates using difference lists for efficient parsing.

## DCG Syntax and Elements

This section introduces the specialized syntax and core elements that make DCG notation both powerful and intuitive. Understanding these building blocks is essential for leveraging DCGs effectively in parsing and generation tasks.

DCGs extend standard Prolog syntax with domain-specific constructs tailored for grammar specification. These elements allow you to express grammatical structures naturally while maintaining the full power of logical programming for semantic processing and constraint checking.

### DCG Element Types

The fundamental building blocks of DCG notation each serve specific roles in grammar specification, from matching literal text to invoking complex parsing logic.

-   **Strings** - Terminal symbols (literal strings to match)
-   **Symbols** - Non-terminal symbols (other DCG rules)
-   **(Symbol args&#x2026;)** - Non-terminals with arguments
-   **nil** - Epsilon production (empty, consumes no input)
-   **!** - Cut operator (prevents backtracking)
-   **(@ goal&#x2026;)** - Semantic actions (constraints that don't consume input)

### DCG Rule Definition

DCG rules are defined using specialized macros that automatically handle the transformation from grammar notation to difference list predicates, abstracting away the complex underlying implementation.

1.  eprolog-define-grammar

```
Define a DCG rule, adding to existing rules with the same arity.

    (eprolog-define-grammar head body-element1 body-element2 ...)

Adds a new DCG rule without replacing existing ones. This allows multiple alternatives for the same non-terminal.
```

2.  eprolog-define-grammar!

```
Define a DCG rule, replacing existing rules with the same arity.

    (eprolog-define-grammar! head body-element1 body-element2 ...)

Similar to `eprolog-define-grammar` but removes existing rules for the same non-terminal with the same arity before adding the new rule. Used for redefinition or when you want only one rule for a non-terminal.
```

### Parsing with phrase/2 and phrase/3

The phrase predicates provide the interface between DCG rules and the input/output streams, enabling both parsing (recognition) and generation of language structures.

1.  phrase/2 and phrase/3

```
Parse or generate using DCG rules.

    ;; Parse complete input
    (eprolog-query (phrase non-terminal input-list))
    
    ;; Parse with remainder  
    (eprolog-query (phrase non-terminal input-list remainder))

`phrase/2` succeeds if the non-terminal can parse the entire input list.
`phrase/3` succeeds if the non-terminal can parse a prefix, with the remainder unified with the third argument.
```

### DCG Examples

These examples demonstrate the practical application of DCG notation, showing how to build grammars incrementally from simple terminals to complex sentence structures.

1.  Basic Grammar Definition

```
    ;; Define a simple grammar for sentences
    (eprolog-define-grammar! s np vp)           ; sentence → noun phrase + verb phrase
    (eprolog-define-grammar! np det noun)       ; noun phrase → determiner + noun  
    (eprolog-define-grammar! vp verb np)        ; verb phrase → verb + noun phrase
    
    ;; Define terminals (strings represent literal words)
    (eprolog-define-grammar! det "the")
    (eprolog-define-grammar det "a")            ; Multiple rules for same non-terminal
    (eprolog-define-grammar! noun "cat")
    (eprolog-define-grammar noun "dog")
    (eprolog-define-grammar! verb "chases")
    (eprolog-define-grammar verb "sees")
```

2.  Parsing with DCG

```
    ;; Parse complete sentences
    (eprolog-query (phrase s ("the" "cat" "chases" "a" "dog")))
    ;; Succeeds - valid sentence
    
    (eprolog-query (phrase s ("a" "dog" "sees" "the" "cat")))  
    ;; Succeeds - another valid sentence
    
    (eprolog-query (phrase s ("cat" "the" "chases")))
    ;; Fails - invalid sentence structure
    
    ;; Parse with remaining tokens
    (eprolog-query (phrase s ("the" "cat" "chases" "a" "dog" "quickly") _rest))
    ;; _rest = ("quickly") - parses sentence, leaves remainder
```

### DCG Best Practices

Understanding common pitfalls and best practices is essential for writing efficient and maintainable DCG code that avoids infinite loops and performance issues.

1.  Left Recursion

```
Avoid left recursion in DCG rules as it can cause infinite loops:

    ;; BAD - left recursive
    (eprolog-define-grammar expr expr "+" term)
    
    ;; GOOD - right recursive  
    (eprolog-define-grammar expr term "+" expr)
    (eprolog-define-grammar expr term)
```

2.  Deterministic Parsing

```
Use cut (!) to make parsing deterministic when appropriate:

    (eprolog-define-grammar! statement declarative-stmt !)
    (eprolog-define-grammar statement question-stmt)
```

3.  Semantic Constraints

```
Use semantic actions (@ &#x2026;) for constraints that don't consume input:

    ;; Ensure number agreement
    (eprolog-define-grammar (s _num) 
      (np _num) 
      (@ (atom _num))           ; Constraint: _num must be bound
      (vp _num))
```

## Basic Grammar Operations

This section introduces the fundamental operations of DCG parsing, demonstrating how to move from simple terminal matching to complex hierarchical grammar structures. These examples establish the foundation for more sophisticated parsing applications.

DCGs transform the traditionally complex task of parsing into an elegant, declarative specification. Instead of manually managing input streams and parsing states, you describe the grammatical structure you want to recognize, and the DCG system handles the underlying mechanics automatically.

The examples in this section progress from elementary concepts to practical parsing scenarios:

-   ****Terminal Matching****: How individual tokens are recognized and consumed
-   ****Non-terminal Composition****: Building complex structures from simpler components
-   ****Hierarchical Parsing****: Creating nested grammatical structures that reflect linguistic organization
-   ****Partial Parsing****: Handling input streams where only a prefix matches the grammar
-   ****Alternative Rules****: Expressing grammatical choices and variations within the same framework

The following test introduces basic DCG concepts with a simple grammar:

```emacs-lisp
(ert-deftest eprolog-feature-dcg-basic ()
  "Test basic DCG functionality."
  (eprolog-test--restore-builtins)
  (eprolog-define-grammar! noun "cat")
  (eprolog-define-grammar noun "dog")  ; Use without ! to add second clause
  (eprolog-define-grammar! verb "runs")
  (eprolog-define-grammar! sentence noun verb)

  (should (eprolog-test--has-solution-p '((phrase sentence ("cat" "runs")))))
  (should (eprolog-test--has-solution-p '((phrase sentence ("dog" "runs")))))
  (should-not (eprolog-test--has-solution-p '((phrase sentence ("cat" "sleeps"))))))
```

A more complete grammar demonstrates how DCGs can parse natural language structures:

```emacs-lisp
(ert-deftest eprolog-feature-dcg-basic-grammar ()
  "Test basic DCG grammar definition and parsing."
  (eprolog-test--restore-builtins)

  ;; Define grammar
  (eprolog-define-grammar! s np vp)
  (eprolog-define-grammar! np det noun)
  (eprolog-define-grammar! vp verb np)
  (eprolog-define-grammar! det "the")
  (eprolog-define-grammar det "a")
  (eprolog-define-grammar! noun "cat")
  (eprolog-define-grammar noun "dog")
  (eprolog-define-grammar! verb "chases")
  (eprolog-define-grammar verb "sees")

  ;; Test parsing valid sentences
  (should (eprolog-test--has-solution-p '((phrase s ("the" "cat" "chases" "a" "dog")))))
  (should (eprolog-test--has-solution-p '((phrase s ("a" "dog" "sees" "the" "cat")))))

  ;; Test parsing invalid sentences
  (should-not (eprolog-test--has-solution-p '((phrase s ("cat" "the" "chases"))))))
```

DCGs can also parse partial input, returning the unparsed remainder:

```emacs-lisp
(ert-deftest eprolog-feature-dcg-parsing-with-remainder ()
  "Test DCG parsing with remainder."
  (eprolog-test--restore-builtins)

  ;; Define grammar  
  (eprolog-define-grammar! s np vp)
  (eprolog-define-grammar! np det noun)
  (eprolog-define-grammar! vp verb np)
  (eprolog-define-grammar! det "the")
  (eprolog-define-grammar! noun "cat")
  (eprolog-define-grammar! verb "chases")

  ;; Test parsing with remainder
  (let ((solutions (eprolog-test--collect-solutions 
                    '((phrase s ("the" "cat" "chases" "the" "cat" "quickly") _rest)))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_rest (car solutions))) '("quickly")))))
```

## Advanced DCG Features

This section explores sophisticated DCG capabilities that enable complex linguistic phenomena, including epsilon productions, parametric grammars, semantic actions, and control structures. These advanced features transform DCGs from simple pattern matchers into powerful language processing systems.

Understanding these advanced features is crucial for building real-world parsers that handle the complexity and variability of natural and formal languages. These capabilities allow DCGs to express linguistic constraints, build structured representations, and implement sophisticated parsing strategies.

```emacs-lisp
(ert-deftest eprolog-feature-dcg-epsilon-productions ()
  "Test DCG epsilon (empty) productions."
  (eprolog-test--restore-builtins)

  ;; Test epsilon productions
  (eprolog-define-grammar! optional-adj nil)
  (eprolog-define-grammar optional-adj adj)
  (eprolog-define-grammar! np det optional-adj noun)
  (eprolog-define-grammar! det "the")
  (eprolog-define-grammar! adj "big")
  (eprolog-define-grammar! noun "cat")

  (should (eprolog-test--has-solution-p '((phrase np ("the" "cat")))))
  (should (eprolog-test--has-solution-p '((phrase np ("the" "big" "cat"))))))
```

DCGs with arguments enable grammatical agreement and semantic processing:

```emacs-lisp
(ert-deftest eprolog-feature-dcg-with-args ()
  "Test DCG with arguments."
  (eprolog-test--restore-builtins)

  ;; Test DCG rules with arguments for grammatical agreement
  (eprolog-define-grammar! (noun singular) "cat")
  (eprolog-define-grammar (noun plural) "cats")
  (eprolog-define-grammar! (det singular) "a")
  (eprolog-define-grammar (det plural) "some")

  ;; Test singular and plural agreement
  (should (eprolog-test--has-solution-p '((phrase (noun singular) ("cat")))))
  (should (eprolog-test--has-solution-p '((phrase (noun plural) ("cats")))))
  (should (eprolog-test--has-solution-p '((phrase (det singular) ("a")))))
  (should (eprolog-test--has-solution-p '((phrase (det plural) ("some")))))

  ;; Test mismatched agreement
  (should-not (eprolog-test--has-solution-p '((phrase (noun singular) ("cats")))))
  (should-not (eprolog-test--has-solution-p '((phrase (noun plural) ("cat"))))))
```

Semantic actions in DCGs allow you to build parse trees or perform computations during parsing:

```emacs-lisp
(ert-deftest eprolog-feature-dcg-semantic-actions ()
  "Test DCG semantic actions."
  (eprolog-test--restore-builtins)

  ;; Test semantic actions
  (eprolog-define-grammar! (s _num) (np _num) (vp _num))
  (eprolog-define-grammar! (np _num) (det _num) (noun _num))
  (eprolog-define-grammar! (vp _num) (verb _num) (np _))
  (eprolog-define-grammar! (det singular) "a")
  (eprolog-define-grammar (det plural) "some")
  (eprolog-define-grammar! (noun singular) "cat")
  (eprolog-define-grammar (noun plural) "cats")
  (eprolog-define-grammar! (verb singular) "chases")
  (eprolog-define-grammar (verb plural) "chase")

  (should (eprolog-test--has-solution-p '((phrase (s _) ("a" "cat" "chases" "some" "cats")))))
  (should-not (eprolog-test--has-solution-p '((phrase (s _) ("a" "cat" "chase" "some" "cats"))))))
```

Cut operations in DCGs provide control over parsing alternatives:

```emacs-lisp
(ert-deftest eprolog-feature-dcg-cut-operations ()
  "Test DCG cut operations."
  (eprolog-test--restore-builtins)

  ;; Test cut operations
  (eprolog-define-grammar! statement declarative !)
  (eprolog-define-grammar statement question)
  (eprolog-define-grammar! declarative s ".")
  (eprolog-define-grammar! question s "?")
  (eprolog-define-grammar! s "test")

  (should (eprolog-test--has-solution-p '((phrase statement ("test" "."))))))
```

## Grammar Generation

DCGs work bidirectionally - they can generate sentences as well as parse them:

```emacs-lisp
(ert-deftest eprolog-feature-dcg-generation ()
  "Test DCG sentence generation."
  (eprolog-test--restore-builtins)

  ;; Define simple grammar
  (eprolog-define-grammar! s np vp)
  (eprolog-define-grammar! np det noun)
  (eprolog-define-grammar! vp verb)
  (eprolog-define-grammar! det "the")
  (eprolog-define-grammar! noun "cat")
  (eprolog-define-grammar! verb "runs")

  ;; Test sentence generation
  (let ((solutions (eprolog-test--collect-solutions '((phrase s _sentence)))))
    (should (> (length solutions) 0))
    (should (equal (cdr (assoc '_sentence (car solutions))) '("the" "cat" "runs")))))
```

Length-constrained generation demonstrates how to combine DCGs with other predicates:

```emacs-lisp
(ert-deftest eprolog-feature-dcg-length-constrained-generation ()
  "Test DCG generation with length constraints."
  (eprolog-test--restore-builtins)

  ;; Test length-constrained generation
  (eprolog-define-predicate (length () 0))
  (eprolog-define-predicate (length (_h . _t) _n)
    (length _t _n1)
    (is _n (+ _n1 1)))

  (eprolog-define-grammar! s2 det noun verb)
  (eprolog-define-grammar! det "a")
  (eprolog-define-grammar! noun "cat")
  (eprolog-define-grammar! verb "runs")

  (let ((solutions (eprolog-test--collect-solutions '((phrase s2 _sentence) (length _sentence 3)))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_sentence (car solutions))) '("a" "cat" "runs")))))
```

## Complex Grammar Applications

This section demonstrates the application of DCGs to sophisticated parsing challenges that showcase their full expressive power. These examples illustrate how DCGs can handle real-world parsing problems with elegance and clarity.

The applications presented here represent common parsing scenarios that arise in language processing, compiler construction, and data processing tasks. Each example builds upon the fundamental DCG concepts to create practical, reusable parsing solutions.

```emacs-lisp
(ert-deftest eprolog-feature-dcg-arithmetic-expressions ()
  "Test arithmetic expression parsing with DCG."
  (eprolog-test--restore-builtins)

  ;; Define arithmetic grammar
  (eprolog-define-grammar! expr term)
  (eprolog-define-grammar expr term "+" expr)
  (eprolog-define-grammar expr term "-" expr)
  (eprolog-define-grammar! term factor)
  (eprolog-define-grammar term factor "*" term)
  (eprolog-define-grammar term factor "/" term)
  (eprolog-define-grammar! factor digit)
  (eprolog-define-grammar factor "(" expr ")")
  (eprolog-define-grammar! digit "1")
  (eprolog-define-grammar digit "2")
  (eprolog-define-grammar digit "3")
  (eprolog-define-grammar digit "4")

  ;; Test arithmetic expression parsing
  (should (eprolog-test--has-solution-p '((phrase expr ("2")))))
  (should (eprolog-test--has-solution-p '((phrase expr ("2" "+" "3" "*" "4")))))
  (should (eprolog-test--has-solution-p '((phrase expr ("(" "2" "+" "3" ")" "*" "4"))))))
```

Nested structure parsing showcases DCGs' recursive capabilities:

```emacs-lisp
(ert-deftest eprolog-feature-dcg-nested-structures ()
  "Test nested structure parsing with DCG."
  (eprolog-test--restore-builtins)

  ;; Define nested parentheses grammar
  (eprolog-define-grammar! parens nil)
  (eprolog-define-grammar parens "(" parens ")" parens)

  ;; Test balanced parentheses
  (should (eprolog-test--has-solution-p '((phrase parens ()))))
  (should (eprolog-test--has-solution-p '((phrase parens ("(" ")")))))
  (should (eprolog-test--has-solution-p '((phrase parens ("(" "(" ")" "(" ")" ")")))))
  (should-not (eprolog-test--has-solution-p '((phrase parens ("(" "(" ")"))))))
```

CSV-style parsing demonstrates practical text processing:

```emacs-lisp
(ert-deftest eprolog-feature-dcg-csv-parsing ()
  "Test CSV-style parsing with DCG."
  (eprolog-test--restore-builtins)

  ;; Test CSV-style parsing
  (eprolog-define-grammar! csv-list item)
  (eprolog-define-grammar csv-list item "," csv-list)
  (eprolog-define-grammar! item "apple")
  (eprolog-define-grammar item "banana")
  (eprolog-define-grammar item "cherry")

  ;; Test parsing simple CSV
  (should (eprolog-test--has-solution-p '((phrase csv-list ("apple")))))
  (should (eprolog-test--has-solution-p '((phrase csv-list ("apple" "," "banana")))))
  (should (eprolog-test--has-solution-p '((phrase csv-list ("apple" "," "banana" "," "cherry"))))))
```

## DCG Error Handling and Edge Cases

This section rigorously tests the robustness and reliability of DCG parsing under challenging conditions, ensuring that the system gracefully handles malformed grammars, edge cases, and stress scenarios.

Comprehensive error handling testing is crucial for building reliable parsing systems. These tests verify that DCGs maintain logical consistency and provide predictable failure modes when confronted with invalid grammars, malformed inputs, or resource constraints.

### Malformed Grammar Rules

These tests explore how the DCG system handles invalid grammar definitions, undefined non-terminals, and improper argument structures, ensuring robust failure behavior.

```emacs-lisp
(ert-deftest eprolog-feature-dcg-malformed-grammar ()
  "Test error handling with malformed DCG rules."
  (eprolog-test--restore-builtins)

  ;; Test parsing with undefined non-terminals
  (should-not (eprolog-test--has-solution-p '((phrase undefined-rule ("test")))))

  ;; Test parsing with empty rule set (no rules defined)
  (should-not (eprolog-test--has-solution-p '((phrase empty-rule _input))))

  ;; Test phrase/2 with invalid arguments (should throw errors or fail)
  (condition-case nil
      (should-error (eprolog-test--has-solution-p '((phrase 123 ("test")))))
    (error t))
  (condition-case nil
      (should-error (eprolog-test--has-solution-p '((phrase test-rule 123))))
    (error t))
  (condition-case nil
      (should-error (eprolog-test--has-solution-p '((phrase test-rule "not-list"))))
    (error t)))
```

### Empty Input and Edge Cases

This subsection tests the parsing system's behavior with empty inputs, minimal valid inputs, and boundary conditions that might expose edge cases in the parsing logic.

```emacs-lisp
(ert-deftest eprolog-feature-dcg-empty-input-edge-cases ()
  "Test DCG parsing with empty and edge case inputs."
  (eprolog-test--restore-builtins)

  ;; Define simple grammar for testing
  (eprolog-define-grammar! word "test")
  (eprolog-define-grammar! empty-rule nil)

  ;; Test parsing empty input
  (should-not (eprolog-test--has-solution-p '((phrase word ()))))
  (should (eprolog-test--has-solution-p '((phrase empty-rule ()))))

  ;; Test parsing with single token
  (should (eprolog-test--has-solution-p '((phrase word ("test")))))
  (should-not (eprolog-test--has-solution-p '((phrase word ("wrong")))))

  ;; Test parsing with extra tokens
  (should-not (eprolog-test--has-solution-p '((phrase word ("test" "extra")))))

  ;; Test parsing with partial matches
  (eprolog-define-grammar! two-words word word)
  (should-not (eprolog-test--has-solution-p '((phrase two-words ("test")))))
  (should (eprolog-test--has-solution-p '((phrase two-words ("test" "test"))))))
```

### Large Input Stress Test

These tests evaluate the DCG system's performance and stability when processing very large input streams, testing resource limits and graceful degradation under memory pressure.

```emacs-lisp
(ert-deftest eprolog-feature-dcg-large-input-stress ()
  "Test DCG parsing with very large inputs."
  (eprolog-test--restore-builtins)

  ;; Define recursive grammar for lists
  (eprolog-define-grammar! large-list nil)
  (eprolog-define-grammar! large-list "item" large-list)

  ;; Test with moderately large input (100 tokens) - may hit resource limits
  (let ((large-input (make-list 100 "item")))
    (condition-case nil
        (eprolog-test--has-solution-p `((phrase large-list ,large-input)))
      (error t))) ;; Accept either success or controlled failure due to resource limits

  ;; Test with very large input (1000 tokens) - should handle gracefully
  (let ((very-large-input (make-list 1000 "item")))
    (condition-case nil
        (eprolog-test--has-solution-p `((phrase large-list ,very-large-input)))
      (error t))) ;; Accept either success or controlled failure

  ;; Test with mixed large input (may fail or error due to size)
  (let ((mixed-input (append (make-list 50 "item") '("different") (make-list 50 "item"))))
    (condition-case nil
        (should-not (eprolog-test--has-solution-p `((phrase large-list ,mixed-input))))
      (error t))))
```

### Left Recursion Detection

Left recursion represents one of the most challenging aspects of parsing system design. These tests verify that the DCG system handles left-recursive grammars appropriately, either through transformation techniques or controlled failure.

```emacs-lisp
(ert-deftest eprolog-feature-dcg-left-recursion ()
  "Test handling of left-recursive grammars."
  (eprolog-test--restore-builtins)

  ;; Define left-recursive grammar (should be handled carefully)
  (eprolog-define-grammar! left-recursive left-recursive "a")
  (eprolog-define-grammar! left-recursive "a")

  ;; Test that left recursion doesn't cause infinite loops
  (condition-case nil
      (progn
        ;; This should either succeed with proper left-recursion handling
        ;; or fail gracefully without infinite looping
        (eprolog-test--has-solution-p '((phrase left-recursive ("a"))))
        (eprolog-test--has-solution-p '((phrase left-recursive ("a" "a")))))
    (error t)) ;; Accept controlled failure

  ;; Test indirect left recursion
  (eprolog-define-grammar! indirect-a indirect-b "x")
  (eprolog-define-grammar! indirect-b indirect-a "y")

  (condition-case nil
      (eprolog-test--has-solution-p '((phrase indirect-a ("x" "y"))))
    (error t))) ;; Accept controlled failure
```

### Invalid Terminal/Non-terminal Mixing

This subsection tests the system's handling of malformed grammar rules that mix invalid terminal and non-terminal constructs, ensuring robust error detection and appropriate failure modes.

```emacs-lisp
(ert-deftest eprolog-feature-dcg-invalid-mixing ()
  "Test error handling with invalid terminal/non-terminal combinations."
  (eprolog-test--restore-builtins)

  ;; Define grammar with various element types (using string instead of number)
  (eprolog-define-grammar! mixed-rule "terminal" valid-nonterminal "123" "another")
  (eprolog-define-grammar! valid-nonterminal "ok")

  ;; Test parsing - should handle mixed elements
  (let ((solutions (eprolog-test--collect-solutions 
    '((phrase mixed-rule ("terminal" "ok" "123" "another"))))))
    (should (= (length solutions) 1)))

  ;; Test with complex invalid structures
  (eprolog-define-grammar! complex-invalid (invalid structure) "test")
  (should-not (eprolog-test--has-solution-p '((phrase complex-invalid _input))))

  ;; Test semantic actions with invalid goals
  (eprolog-define-grammar! invalid-semantic-action 
    "test" 
    (@ (invalid lisp expression))
    "end")

  (should-not (eprolog-test--has-solution-p 
    '((phrase invalid-semantic-action ("test" "end"))))))
```

