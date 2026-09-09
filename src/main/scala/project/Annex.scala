/** Annex.scala Provides utility functions for propositional logic formulas,
  * including extracting variables and generating models. Authors:
  *   - Leslie Lucynda TINGUE
  *   - Bruno SANDELE
  *   - Dave Ronic DONKENG
  * version 1.0
  */
package project

import project.Formula
import project.TableauBuilder.*
import project.{LazyTree, Tree, Status, Model}

object Annex {

  /** Extract all variables from a formula.
    *
    * @param f
    *   The formula to analyze
    * @return
    *   A set of variable names (Strings) appearing in the formula
    */
  def allVariables(f: Formula): Set[String] = f match
    case Formula.Prop(name)    => Set(name)
    case Formula.Not(n)        => allVariables(n)
    case Formula.And(l, r)     => allVariables(l) ++ allVariables(r)
    case Formula.Or(l, r)      => allVariables(l) ++ allVariables(r)
    case Formula.Implies(l, r) => allVariables(l) ++ allVariables(r)
    // Bonus
    case Formula.Equivalent(l, r) => allVariables(l) ++ allVariables(r)
    case Formula.Xor(l, r)        => allVariables(l) ++ allVariables(r)
    case Formula.Xnor(l, r)       => allVariables(l) ++ allVariables(r)
    case Formula.Nand(l, r)       => allVariables(l) ++ allVariables(r)

    // n-ary operators
    case Formula.AndN(l) => l.flatMap(allVariables).toSet
    case Formula.OrN(l)  => l.flatMap(allVariables).toSet

  /** Generate all possible models (valuations) for a given set of formulas.
    *
    * This function first extracts variables already constrained by the
    * formulas, then generates all combinations of True/False for any missing
    * variables.
    *
    * @param f
    *   A set of formulas
    * @param variables
    *   The set of all variable names that must appear in the models
    * @return
    *   A set of all possible models (Map[String, Boolean])
    */
  def createModel(f: Set[Formula], variables: Set[String]): Set[Model] =

    /** Extract variables explicitly assigned by a formula and update a partial
      * model.
      *
      * @param form
      *   The formula to extract from
      * @param m
      *   The partial model to update
      * @param value
      *   The Boolean value to assign if the formula is a simple proposition
      * @return
      *   An updated model with the variable assigned, or unchanged if no direct
      *   assignment
      */
    def findVariable(form: Formula, m: Model, value: Boolean): Model =
      form match
        case Formula.Prop(name) => m + (name -> value)
        case Formula.Not(not)   => findVariable(not, m, !value)
        case _                  => m // other forms do not impose values

    // Build an incomplete model using ALL formulas in f
    val incompletModel: Model =
      f.foldLeft(Map.empty[String, Boolean]) { (acc, formula) =>
        findVariable(formula, acc, true)
      }

    val missing = variables.filter(v => !incompletModel.contains(v))

    /** Generate all combinations for missing variables.
      *
      * @param current
      *   A partial model
      * @param vars
      *   List of remaining variables to assign
      * @return
      *   A set of all models extending `current` with all combinations of
      *   True/False for `vars`
      */
    def fillMissing(current: Model, vars: List[String]): Set[Model] =
      if vars.isEmpty then Set(current)
      else
        val v = vars.head
        val rest = vars.tail
        fillMissing(current + (v -> true), rest) ++ fillMissing(
          current + (v -> false),
          rest
        )

    fillMissing(incompletModel, missing.toList)
}
