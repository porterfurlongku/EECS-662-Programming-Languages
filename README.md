# Racket Interpreter Project

## Overview

This project implements an interpreter for a simple programming language in Racket. The interpreter includes modules for parsing, abstract syntax tree (AST) representation, interpretation, and type checking. The key files in this project are:

- `ast.rkt`: Defines the abstract syntax tree (AST) structure.
- `interp.rkt`: Implements the interpreter.
- `main.rkt`: The main entry point for running the interpreter.
- `parser.rkt`: Handles parsing of the input source code.
- `type.rkt`: Implements type checking for the language.

## Features

This interpreter supports the following features:
- **Primitives**: Basic operations such as arithmetic and boolean operations.
- **Conditionals**: `if` expressions and `cond` statements.
- **Let Bindings**: Local variable bindings using `let`.
- **Arity Checking**: Ensuring functions are called with the correct number of arguments.
- **Term Rewrites**: Rewriting expressions for simplification, including `cond` to nested `if` and currying of functions.
- **Lambda Calculus**: Support for lambda abstractions and applications.
- **Type Checking**: Ensuring expressions adhere to the defined type rules.

## Installation

To run this project, you need to have Racket installed. You can download and install Racket from [the official website](https://racket-lang.org/).

## File Descriptions

### `ast.rkt`
This file defines the structure of the abstract syntax tree (AST) used by the interpreter. The AST is a representation of the parsed source code, capturing its syntactic structure in a hierarchical format.

### `interp.rkt`
This file contains the implementation of the interpreter. It defines the evaluation functions that traverse the AST and execute the corresponding operations.

### `main.rkt`
The main entry point for the interpreter. This file handles the command-line interface, reads the input source code, and invokes the parser, type checker, and interpreter.

### `parser.rkt`
This file is responsible for parsing the input source code into an AST. It defines the grammar of the language and converts source code text into the corresponding AST nodes.

### `type.rkt`
This file implements the type checking functionality for the language. It ensures that the source code adheres to the language's type rules before execution.

## Examples
### 'Example Conditionals'
```sh
(cond [(zero? (- 6 5)) 1]
      [(<= 6 7)        2]
      [else            3])
```
### 'Example Primitives'
```sh
(or e e)
(- e)
(not e)
(% e e)
```
### 'Example Let Bindings'
```sh
(let ((x 1)
      (y 2))
  (+ x y))
```
### 'Example Arity Checking -> This Will Result in Error'
```sh
(define (foo a b)
  (+ a b))

(foo 1)
```
### 'Example Term Rewrites'
```sh
(cond [(zero? (- 6 5)) 1]
      [(<= 6 7)        2]
      [else            3])
```
Can Be Written As:
```sh
(if (zero? (- 6 5))
    1
    (if (<= 6 7)
        2
        3))
```
