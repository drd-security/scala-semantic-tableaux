# Semantic Tableaux in Scala

A modular Scala implementation of **semantic tableaux for propositional logic**, with both strict and lazy trees, model/counterexample generation, advanced logical operators, n-ary conjunction/disjunction, and Graphviz/DOT visualization.

## Features

- Propositional formulas represented with Scala algebraic data types.
- Semantic-tableau elimination rules for classical operators.
- Strict tree construction.
- Lazy tree construction that evaluates branches on demand.
- Satisfiability, validity, tautology, and contradiction checks.
- Model enumeration and counterexample generation.
- Extended operators: equivalence, XOR, NAND, XNOR.
- N-ary `AND` and `OR` operators.
- DOT output for graph visualization.
- Executable demonstration/test objects for strict, lazy, and extension behavior.

## Architecture

```text
Formula.scala
      |
EliminationRule.scala
      |
Tableau.scala <---- TableauBuilder.scala
      |                    |
      |               strict + lazy construction
      |
Bibli.scala / LazyBibli.scala
      |
Annex.scala        TableauVisualizer.scala
      |                    |
models/helpers            DOT output
```

## Run with scala-cli

From the repository root:

```bash
scala-cli run src/main/scala --main-class project.TestSemtab
scala-cli run src/main/scala --main-class project.TestlazySemtab
scala-cli run src/main/scala --main-class project.TestExtension
```

## Example capabilities

The public API can answer questions such as:

- Is a set of formulas satisfiable?
- Is a formula valid under a set of hypotheses?
- Is a formula a tautology or contradiction?
- Which models satisfy the formula set?
- Which counterexamples invalidate a conclusion?

It can also emit a DOT representation of the generated tableau, including branch status and elimination-rule labels.

## Academic context and contribution

Three-person functional-programming project. I completed the majority of implementation/integration work, while both teammates also contributed. The source files retain original team attribution.

## Design focus

The project emphasizes immutable values, pattern matching, recursive tree construction, algebraic data types, modular functions, and laziness rather than imperative stateful algorithms.

## Publication status

The original French assignment/report are excluded; this repository documents the work in English. See [NOTICE.md](NOTICE.md).
