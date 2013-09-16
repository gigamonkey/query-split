query-split
===========

September 2013 coding challenge

Write a program whose input is a boolean expression (some combination of ANDs, ORs, NOTs, literal true and false values, and boolean variables) some of whose variables are cheap to access and some of which are expensive to access. (We'll in fact use the simplest cost model: cheap variables are free and expensive variables are infinitely expensive.) The output of this program is a new boolean expression that uses only the cheap variables and which returns true whenever the original would (i.e. for any specific set of values for the variables) and which returns false as often as it can when the original would.

(The motivation for this challenge is things like this: imagine a query that joins data sets and then filters the result. The filter predicate may access variables from both sides of the join but it may be a win to perform a pre-filter on each side of the join first to weed out rows before the join is performed. The pre-filter predicates obviously can only use the variables that are present on one side of the join.)

In slightly more formal terms, given a function f of n variables, the first k < n of which are cheap, you need to produce g such that:

    f(v1,v2,..vn) = g(v1,v2,...vk) && f(v1,v2,..vn)

Or, to put it another way:

    !g(v1,v2,...vk) implies !f(v1,v2,...vn)

For purposes of this challenge, you need to write a program that can parse the following grammar:

    formula     := variable | literal | expression
    variable    := cheap | expensive
    cheap       := v[0-9]+
    expensive   := w[0-9]+
    literal     := "T" | "F"
    expression  := conjunction | disjunction | negation
    conjunction := "(and" ws formula ws formula ")"
    disjunction := "(or" ws formula ws formula ")"
    negation    := "(not" ws formula ")"
    ws          := " "+

Then write a program that takes a file containing expressions in the form, one per line, and output a file containing for each input expression a pre-filter that uses only the cheap variables.

Your entry is disqualified for any of your pre-filters, g:

    input.exists { x => f(x) != (g(x) && f(x)) } // i.e. must be correct

And correct pre-filters should maximize:

    input.count { x => !g(x) }

All other things being equal, smaller pre-filters (measured by tree size of the expression) beat bigger ones. Style points are awarded for efficient computation and general cleverness. The grand prize is bragging rights and the satisfaction of a job well done.
