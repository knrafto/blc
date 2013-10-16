# BLC

A functional, referentially transparent, untyped, interactive language based
on the lambda calculus.

## Expressions

Basic lambda calculus. Identifiers may not be "include", "let", "in", or "=",
and may not contain whitespace or the characters ";\.#()".

### Let-expressions

A let-expression is of the form

    let <name1> = <exp1>;
        <name2> = <exp2>;
        ...
        <nameN> = <expN>[;] in <exp>

and is equivalent to

    \<name1> (\<name2> ... ((\<nameN>. <exp>) <expN>) ... <exp2>) <exp1>

Thus, because variables refer to their innermost scope, variables in the
definitions refer to latest definition that appears before the expression.
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

## Imports

And import statement of the form

    import path.to.file;

will inline the file path/to/file.blc, similar to the C preprocessor.
Thus, order of imports can matter in some instances.

The standard prelude (Prelude.blc) is automatically imported before every
file.

## Execution

After all imports are inlined into a single list of declarations, the main
expression is equivalent to

    let <decls> in main

where `let` is evaluated as above. The resulting expression is applied to
`stdin`, and should evaluate to `stdout`. `stdin` and `stdout` are both
represented as lists of bytes (see the prelude) for details.

## Implementation

The core lambda calculus interpreter (specifically, avoiding variable capture)
was inspired by (Edward Kmett's post "Bound" on FP Complete)
[https://www.fpcomplete.com/user/edwardk/bound].
