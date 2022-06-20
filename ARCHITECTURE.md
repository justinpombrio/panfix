# Architecture (planned)

Parse using [Operator Precedence Grammars
(OPGs)](https://en.wikipedia.org/wiki/Operator-precedence_grammar)
They allow parsing a source file by chunk in parallel!

1. A `Source` splits a source string into lines and records the byte offset of
   the lines to allow computing line/col info later.
2. A `Grammar` takes a set of operator definitions and constructs the relevant
   tables: token and precedence stuff.
3. A `Lexer` takes a source and produces an iterator of lexemes. (A lexeme has a
   token integer & a span.)
4. `resolve` takes an iterator of lexemes and produces a vector of lexemes or
   error. It (i) inserts Blanks and Juxtaposes, and (ii) resolves e.g. unary vs.
   binary `-`.
5. `shunt` takes an iterator of lexems and produces an iterator of lexemes,
   re-ordered by the algorithm on Wikipedia to be in RPN order.
6. We map tokens to Ops, tossing out any token that isn't the start of an op.
7. `TreeVisitor` takes the RPN sequence and converts it into a navigable tree
   (while only allocating two vectors). It is wrapped in `ParseTree` to have a
   nicer interface.
