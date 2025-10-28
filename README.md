# CS2104 Scheme Syntax Checker

## Overview
This project implements a **recursive syntax checker** for a subset of Scheme defined by the course’s BNF grammar.  
It verifies that Scheme programs conform to the given syntax rules and collects detailed error messages instead of stopping after the first error.  
The checker is **self-applicable**, meaning it can successfully verify its own source code.

---

## Project Structure

CS2104/
├── syntax_checker_with_error_log.scm # Final checker with detailed error reporting
├── syntax_checker_with_true_false.scm # Simplified checker returning #t/#f
├── old_version_checker.scm # Early version (pre-refactor)
│
├── test_program/
│ ├── good_prog.scm # Collection of valid Scheme programs
│ ├── all_bad.scm # Collection of invalid programs
│ ├── function_test.scm # Tests for expression-level components
│ ├── cyclic_test.scm # Tests for cyclic value detection
│ ├── p1.scm – p4.scm # Progressive smaller unit tests
│
└── README.md

## How to Run
Load the checker in your Scheme REPL:
```scheme
(load "syntax_checker_with_error_log.scm")
```

Run on a file:

```scheme
(check-file "test_program/good_prog.scm")
(check-file "test_program/all_bad.scm")
```

Test all unit functions of syntax checker:

```scheme
(load "syntax_checker_with_error_log.scm")
(load "test_program/function_test.scm")
(run-tests)
```

## Possible Extensions

Line Tracking: Attach line numbers to each parsed expression for clearer error messages in (check-file).

Full Recursive Checking: Replace shallow checks with a deep descent for more granular syntax validation.

Scheme Formatter: Develop a structural formatter that enforces consistent indentation and style, addressing collaboration issues with inconsistent code layout.

Semantic Checking: Add variable scope and binding checks to move toward a linter-style tool.