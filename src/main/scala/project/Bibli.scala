/** Bibli.scala Provides functions to check satisfiability, validity, tautology,
  * contradiction, models and counter-examples of propositional logic formulas
  * using semantic tableaux. Authors:
  *   - Leslie Lucynda TINGUE
  *   - Bruno SANDELE
  *   - Dave Ronic DONKENG
  * version 1.0
  */
package project

import project.Formula
import project.Annex.*
import project.TableauBuilder.*
import project.{Tree, Status, Model}

object Bibli {

  /** Check if a set of formulas is satisfiable using semantic tableaux.
    *
    * @param s
    *   A set of formulas to check for satisfiability
    * @return
    *   True if the set is satisfiable, false otherwise
    */
  def isSatisfiable(s: Set[Formula]): Boolean =
    models(s).nonEmpty

  /** Check if a formula is valid under a set of hypotheses using semantic
    * tableaux.
    *
    * @param s
    * @param f
    */
  def isValid(s: Set[Formula], f: Formula): Boolean =
    !isSatisfiable(s + Formula.Not(f))

  /** Check if a formula is a tautology using semantic tableaux.
    *
    * @param f
    */
  def isTautology(f: Formula): Boolean =
    !isSatisfiable(Set(Formula.Not(f)))

  /** Check if a formula is a contradiction using semantic tableaux.
    *
    * @param f
    */
  def isContradiction(f: Formula): Boolean =
    !isSatisfiable(Set(f))

  /** Generate all models from a formula tree (tableau tree).
    *
    * @param s
    *   A set of formulas representing the root of the tableau
    * @return
    *   A set of all models (Map[String, Boolean]) consistent with the tableau
    */
  def models(s: Set[Formula]): Set[Model] =
    val t = semtab(s)
    val variables = s.flatMap(allVariables)

    /** Recursively traverse a tableau tree to generate all models.
      *
      * @param t
      *   The subtree to process
      * @return
      *   A set of models consistent with this subtree
      */
    def allModel(t: Tree[Set[Formula]]): Set[Model] = t match
      // Open leaf: generate all models completing its partial valuation
      case Tree.Leaf(formulas, Status.Open) =>
        createModel(formulas, variables)

      // Closed leaf: no models
      case Tree.Leaf(_, Status.Closed) =>
        Set.empty

      // Branch: union of all child models
      case Tree.Branch(_, children) =>
        children.flatMap(allModel).toSet

    allModel(t)

  /** Generate counterexamples for a formula under a given set of hypotheses.
    *
    * A counterexample is a model where all formulas in `s` are true but `f` is
    * false.
    *
    * @param s
    *   A set of formulas representing the hypotheses
    * @param f
    *   A formula for which we want to find counterexamples
    * @return
    *   A set of models (Map[String, Boolean]) where `s` holds but `f` is false
    */
  def counterExamples(s: Set[Formula], f: Formula): Set[Model] =
    models(s + Formula.Not(f))
}
