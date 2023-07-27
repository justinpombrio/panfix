# Architecture

Panfix parsing.

1. A `Grammar` takes a set of operator definitions and constructs the relevant
   tables: token and precedence stuff.
2. A `Source` splits a source string into lines and records the byte offset
   of each line. This allows getting the contents of the n'th line (needed
   for displaying parse errors), which is otherwise quite difficult.
3. A `Lexer` takes a source and produces an iterator of lexemes. (A lexeme has a
   token integer & a span.)
4. `resolve` takes an iterator of lexemes and produces a vector of lexemes or
   error. It (i) inserts Blanks and Juxtaposes, and (ii) resolves e.g. unary vs.
   binary `-`.
5. `shunt` takes an iterator of lexems and produces an iterator of lexemes,
   re-ordered by the [shunting yard algorithm](
   https://en.wikipedia.org/wiki/Shunting_yard_algorithm)
   to be in RPN order.
6. We filter out any token that isn't the first token of its op.
7. `TreeVisitor` takes the RPN sequence and converts it into a navigable tree
   (while only allocating two vectors). It is wrapped in `ParseTree` to have a
   nicer interface.

Note that it's _nearly_ possible to parse a source file by chunk in parallel, if
you first scan for newlines and assume that a token never spans multiple lines.
