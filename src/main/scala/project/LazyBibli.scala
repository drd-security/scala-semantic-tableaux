/** LazyBibli.scala Implements lazy semantic tableaux methods for propositional
  * logic formulas. Authors:
  *   - Leslie Lucynda TINGUE
  *   - Bruno SANDELE
  *   - Dave Ronic DONKENG
  * version 1.0
  */
package project

import project.Formula
import project.Annex.*
import project.TableauBuilder.*
import project.{LazyTree, Tree, Status, Model}

object LazyBibli {

  /** Check if a set of formulas is satisfiable using lazy semantic tableaux.
    *
    * @param s
    *   A set of formulas to check for satisfiability
    * @return
    *   True if the set is satisfiable, false otherwise
    */
  def lazyIsSatisfiable(s: Set[Formula]): Boolean =
    lazyModels(s).nonEmpty

  /** Check if a formula is valid under a set of hypotheses using lazy semantic
    * tableaux.
    *
    * @param s
    * @param f
    */
  def lazyIsValid(s: Set[Formula], f: Formula): Boolean =
    !lazyIsSatisfiable(s + Formula.Not(f))

  /** Check if a formula is a tautology using lazy semantic tableaux.
    *
    * @param f
    */
  def lazyIsTautology(f: Formula): Boolean =
    !lazyIsSatisfiable(Set(Formula.Not(f)))

  /** Check if a formula is a contradiction using lazy semantic tableaux.
    *
    * @param f
    */
  def lazyIsContradiction(f: Formula): Boolean =
    !lazyIsSatisfiable(Set(f))

  /** Generate all models from a lazy formula tree (tableau tree).
    *
    * @param s
    *   A set of formulas representing the root of the tableau
    * @return
    *   A set of all models (Map[String, Boolean]) consistent with the tableau
    */
  def lazyModels(s: Set[Formula]): Set[Model] =
    val t = lazysemtab(s)
    val variables = s.flatMap(allVariables)

    // Use openLeavesLazyList to retrieve the open leaves
    t.openLeavesLazyList.flatMap(fs => createModel(fs, variables)).toSet

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
  def lazyCounterExamples(s: Set[Formula], f: Formula): Set[Model] =
    lazyModels(s + Formula.Not(f))
}
