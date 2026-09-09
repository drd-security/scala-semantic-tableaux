/** EliminationRule.scala Defines the elimination rules for propositional logic
  * formulas in a tableau method. Authors:
  *   - Leslie Lucynda Tingue
  *   - Bruno Sandele
  *   - Dave Ronic Donkeng
  * version 1.0
  */
package project

import project.Formula.*
import project.{LazyTree, Tree, Status, Model}

//Enumeration of elimination rules for propositional logic formulas.
enum EliminationRule:
  case DoubleNegation // NOT(NOT( phi))
  case OrRule // phi OR psi
  case AndRule // phi AND psi
  case ImpliesRule // phi -> psi
  case NotOrRule // NOT(phi OR psi)
  case NotAndRule // NOT(phi AND psi)
  case NotImpliesRule // NOT(phi -> psi)

  // Bonus
  case EquivRule // phi <-> psi
  case XorRule // phi XOR psi
  case NandRule // phi NAND psi
  case XnorRule // phi XNOR psi

  // n-ary operators
  case AndNRule // phi_1 AND phi_2 AND ... AND phi_n
  case OrNRule // phi_1 OR phi_2 OR ... OR phi_n

object EliminationRule:

  /** Check if a set of formulas contains a contradiction.
    * @param formula
    */
  def hasContradiction(formula: Set[Formula]): Boolean =
    val props = formula.collect { case Prop(name) => name }
    props.exists { p =>
      formula.contains(Prop(p)) && formula.contains(Not(Prop(p)))
    }

  /** Check if a formula is a basic formula (propositional variable or its
    * negation).
    * @param formula
    */
  def basicFormula(formula: Formula): Boolean = formula match
    case Prop(_)      => true
    case Not(Prop(_)) => true
    case _            => false

  /** Find a formula to eliminate from a set of formulas based on elimination
    * rules.
    * @param formulas
    */
  def FormulaToEliminate(formulas: Set[Formula]): Option[Formula] =
    formulas.find {
      case Not(Not(_))        => true
      case Or(_, _)           => true
      case And(_, _)          => true
      case Implies(_, _)      => true
      case Not(Or(_, _))      => true
      case Not(And(_, _))     => true
      case Not(Implies(_, _)) => true

      // Bonus
      case Equivalent(_, _) => true
      case Xor(_, _)        => true
      case Nand(_, _)       => true
      case Xnor(_, _)       => true

      // n-ary operators
      case AndN(_) => true
      case OrN(_)  => true

      case _ => false
    }

  /** Applies the correct elimination rule to a given formula.
    *
    * Returns:
    *   - the elimination rule used
    *   - a list of child formula sets
    *
    * @param f
    *   the formula to eliminate
    * @param fs
    *   the current set of formulas in the branch
    * @return
    *   (EliminationRule, List[Set[Formula]])
    */
  def applyEliminationRule(
      f: Formula,
      fs: Set[Formula]
  ): (EliminationRule, List[Set[Formula]]) =
    f match
      case Not(Not(p)) =>
        (DoubleNegation, List(fs + p))

      case Or(p, q) =>
        (OrRule, List(fs + p, fs + q))

      case And(p, q) =>
        (AndRule, List(fs + p + q))

      case Implies(p, q) =>
        (ImpliesRule, List(fs + Not(p), fs + q))

      case Not(Or(p, q)) =>
        (NotOrRule, List(fs + Not(p) + Not(q)))

      case Not(And(p, q)) =>
        (NotAndRule, List(fs + Not(p), fs + Not(q)))

      case Not(Implies(p, q)) =>
        (NotImpliesRule, List(fs + p + Not(q)))

      // BONUS
      case Equivalent(p, q) =>
        (EquivRule, List(fs + Implies(p, q), fs + Implies(q, p)))

      case Xor(p, q) =>
        (XorRule, List(fs + Or(And(p, Not(q)), And(Not(p), q))))

      case Nand(p, q) =>
        (NandRule, List(fs + Not(And(p, q))))

      case Xnor(p, q) =>
        (XnorRule, List(fs + Equivalent(p, q)))

      // n-ary operators
      case AndN(p) if p.size >= 1 =>
        (AndNRule, List(fs ++ p.toSet))

      case OrN(p) if p.size >= 1 =>
        (OrNRule, p.map(p => fs + p).toList)

      case _ =>
        throw new IllegalArgumentException(
          s"No elimination rule for formula: $f"
        )

  /** Applies the correct elimination rule to a given formula lazily.
    *
    * Returns:
    *   - the elimination rule used
    *   - a LazyList of child formula sets
    *
    * @param f
    *   the formula to eliminate
    * @param fs
    *   the current set of formulas in the branch
    * @return
    *   (EliminationRule, LazyList[Set[Formula]])
    */
  def applyEliminationRuleLazy(
      f: Formula,
      fs: Set[Formula]
  ): (EliminationRule, LazyList[Set[Formula]]) =
    f match
      case Not(Not(p)) =>
        (DoubleNegation, LazyList(fs + p))

      case Or(p, q) =>
        (OrRule, LazyList(fs + p, fs + q))

      case And(p, q) =>
        (AndRule, LazyList(fs + p + q))

      case Implies(p, q) =>
        (ImpliesRule, LazyList(fs + Not(p), fs + q))

      case Not(Or(p, q)) =>
        (NotOrRule, LazyList(fs + Not(p) + Not(q)))

      case Not(And(p, q)) =>
        (NotAndRule, LazyList(fs + Not(p), fs + Not(q)))

      case Not(Implies(p, q)) =>
        (NotImpliesRule, LazyList(fs + p + Not(q)))

      // Bonus connectives
      case Equivalent(p, q) =>
        (EquivRule, LazyList(fs + Implies(p, q), fs + Implies(q, p)))

      case Xor(p, q) =>
        (XorRule, LazyList(fs + Or(And(p, Not(q)), And(Not(p), q))))

      case Nand(p, q) =>
        (NandRule, LazyList(fs + Not(And(p, q))))

      case Xnor(p, q) =>
        (XnorRule, LazyList(fs + Equivalent(p, q)))

      // n-ary operators
      case AndN(p) if p.size >= 1 =>
        (AndNRule, LazyList(fs ++ p.toSet))

      case OrN(p) if p.size >= 1 =>
        (OrNRule, p.to(LazyList).map(p => fs + p))

      case _ =>
        throw new IllegalArgumentException(
          s"No elimination rule for formula: $f"
        )
