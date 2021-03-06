# JB Interpreter
JB is programming language designed for  Programming Languages and Paradigms university course.
It has C like syntax and contains a few features I thought would be nice to implement:
* Labeled loops, which allow to break out of nested loop at any level
* Variants, also known as tagged unions, which make it easier to model disjoint sum types

More specific description of language syntax and semantics is located in project.pdf.

# Building and running interpreter
* Building interpreter:	`make`
* Running tests: `make test`
* Executing interpreter with custom program: `./interpreter good/example.jb`

Tests included are divided into two parts -- good and bad -- according to task.
* Good programs are tested by checking both exit code and `stdout`.
  To pass the test program has to return with exit code 0 and its `stdout` has to
  match contents of corresponding *.out file.
* Bad programs are only tested if their exit code is equal to interpreter error code (255).
  For each such test interpreter message is printed and can be manually checked if it
  matches expectations.


# Description of solution
Interpretation of program happens in three stages.

Firstly, source code is parsed to AST. Code responsible for parsing is generated
automatically by bnfc, from grammar in file bnfc/Grammar.cf.

After successful parsing, type checking is conducted. Type checker maintains state
with four elements:
* `tcVEnv` -- mapping from variable names to their corresponding types
* `tcFEnv` -- list of functions visible at given moment
* `tcTypes` -- list of types defines by user so far
* `rcRetType` -- return type of currently checked function

During checking, above information is used to determine correctness of all
statements and values. Most of type checking is pretty straightforward, although
due to non trivial type system of my language there was a need to introduce quite
complicated function `mergeTypes`. Its purpose is to find the smallest
type, which is a superset of two given types or signalize that no such type exists.
For example, if two records have the same field sets, then their merge type is
a record with the same fields sets, where type of each field if merge of its types
from both records.

Finally, if type checking finishes with success, proper interpretation begins.
Interpreter state contains environments of variables and functions,
current `break`/`continue`/`return` state, first not used location, store for keeping variables values
and output produced so far by program. After all instructions are interpreted, the output
is printed to `stdout`.

I chose this approach, because it seems elegant to me that `IO ()` monad is used only
in top-level function and is not present in interpreter logic. The downside is that
if program enters infinite loop, no output is produced at all.


# Differences between language declaration and actual implementation
* During implementation I realized that passing arbitrary value by reference does not
  make sense in general (for example `f(&1)`), so I decided to restrict passing by
  reference only to variables.
* Minor syntactic change: to avoid parsing conflicts I changed records initializing
  syntax from
  `my_record a = { numer => 1; };`
  to
  `my_record a = < numer => 1; >;`

Program which corresponds to example program from language declaration, including
changes specified above, is included in file `good/example.jb`.
