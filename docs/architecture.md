# Architecture

## Formula representation

`Formula` is an algebraic data type representing propositions, negation, binary connectives, the bonus operators, and n-ary conjunction/disjunction. Pattern matching is used throughout the project to keep rule handling explicit and type-directed.

## Elimination rules

`EliminationRule` selects a non-basic formula and expands it into one or more child formula sets according to semantic-tableau rules. Contradictions close a branch when a proposition and its negation appear together.

## Strict and lazy trees

The strict builder recursively constructs the complete tableau. The lazy builder delays child evaluation, allowing consumers to inspect only the branches they need.

## Logic queries

`Bibli` and `LazyBibli` expose higher-level operations such as satisfiability, validity, models, tautology detection, contradiction detection, and counterexamples.

## Visualization

`TableauVisualizer` serializes a strict tableau to Graphviz DOT, annotating nodes with formulas and edges with elimination-rule names. Open/closed leaves are visually distinguishable when rendered.
