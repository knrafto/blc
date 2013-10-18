# BLC

A functional, referentially transparent, untyped, interactive language based
on the lambda calculus.

## Syntax

Basic lambda calculus syntax, with lambda represented as a backslash.
Identifiers may not be `include`, `let`, `in`, or `=`, may not contain
whitespace or the characters `\.#();`, and additionally may not start with
any of `'"`.

### Let-expressions

Let-expression are syntactic sugar for binding variables. A let-expression
is of the form

    let <name1> = <exp1>;
        <name2> = <exp2>;
        ...
        <nameN> = <expN>[;] in <exp>

and is equivalent to

    \<name1> (\<name2> ... ((\<nameN>. <exp>) <expN>) ... <exp2>) <exp1>

Thus, because variables in lambda calculus refer to their innermost scope,
variables are bound to latest definition that appears before the expression.
For example,

    let a = 1; b = a; a = 2 in + a b;

will evaluate to `3`.

## Files

Comments begin with a single hash character and continue to the end
of a line.

A file is a semicolon-separated list of import statements and declarations
of the form

    <name> = <exp>;

Names may be redefined at any time. A name appearing in an expression refers
to the the latest definition that appears before the expression
(see "Execution").

## Execution

The main expression is equivalent to

    let <decls> in main

where `let` is evaluated as above. The resulting expression is applied to
`stdin`, and should evaluate to `stdout`. `stdin` and `stdout` are both
represented as lists of bits.
