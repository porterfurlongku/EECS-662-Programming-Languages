# Racket Interpreter Project

## Overview

This project implements an interpreter for a simple programming language in Racket. The interpreter includes modules for parsing, abstract syntax tree (AST) representation, interpretation, and type checking. The key files in this project are:

- `ast.rkt`: Defines the abstract syntax tree (AST) structure.
- `interp.rkt`: Implements the interpreter.
- `main.rkt`: The main entry point for running the interpreter.
- `parser.rkt`: Handles parsing of the input source code.
- `type.rkt`: Implements type checking for the language.

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
You can find example source code files in the `examples` directory. To run an example, use the following command:

```sh
racket main.rkt examples/example1.txt
