/** Formula.scala Defines the propositional logic formulas used in the tableau
  * method. Authors:
  *   - Leslie Lucynda TINGUE
  *   - Bruno SANDELE
  *   - Dave Ronic DONKENG
  * version 1.0
  */
package project

/** Represents propositional logic Formula. basic constructors are :
  *   - Prop(name) : propositional variable
  *   - Not(phi) : negation
  *   - And(phi, psi) : conjunction
  *   - Or(phi, psi) : disjunction
  *   - Implies(antecedent, consequent) : implication
  *
  * bonus constructors are :
  *   - Equivalent(phi, psi) : implication
  *   - Xor(phi, psi) : exclusive or
  *   - Nand(phi, psi) : not and
  *   - Xnor(phi, psi) : not exclusive or
  */
enum Formula:
  case Prop(name: String)
  case Not(phi: Formula)
  case And(phi: Formula, psi: Formula)
  case Or(phi: Formula, psi: Formula)
  case Implies(phi: Formula, psi: Formula)

  // bonus constructors
  case Equivalent(phi: Formula, psi: Formula)
  case Xor(phi: Formula, psi: Formula)
  case Nand(phi: Formula, psi: Formula)
  case Xnor(phi: Formula, psi: Formula)

  // n-ary operators
  case AndN(phi: List[Formula]) // And for n>=1 arguments
  case OrN(phi: List[Formula]) // Or for n>=1 arguments

object Formula:
  /** Helper method to create a conjunction of multiple formulas. */
  def andAll(formulas: Formula*): Formula =
    formulas.toList match
      case Nil =>
        throw new IllegalArgumentException(
          "At least one formula is required for AndN."
        )
      case head :: Nil => head
      case list        => AndN(list)

  /** Helper method to create a disjunction of multiple formulas. */
  def orAll(formulas: Formula*): Formula =
    formulas.toList match
      case Nil =>
        throw new IllegalArgumentException(
          "At least one formula is required for OrN."
        )
      case head :: Nil => head
      case list        => OrN(list)
