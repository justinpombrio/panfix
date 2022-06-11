# Architecture (planned)

Parse using [Operator Precedence Grammars
(OPGs)](https://en.wikipedia.org/wiki/Operator-precedence_grammar)
They allow parsing a source file by chunk in parallel!

1. A `Source` splits a source string into lines and records the byte offset of
   the lines to allow computing line/col info later.
?. A `Grammar` takes a set of operator definitions and constructs the relevant
   tables: token and precedence stuff.
2. A `Lexer` takes a source and produces an iterator of lexemes. (A lexeme has a
   token integer & a span.)
3. `resolve` takes an iterator of lexemes and produces an iterator of lexemes.
   It (i) inserts Blanks and Juxtaposes, and (ii) resolves e.g. unary vs. binary
   `-`.
4. `shunt` takes an iterator of lexems and produces an iterator of lexemes,
   re-ordered by the algorithm on Wikipedia to be in RPN order.
5. `TreeVisitor` takes the RPN sequence and converts it into a navigable tree
   (while only allocating two vectors).
