/** Tableau Visualizer. provides a method to visualize a tableau tree as a DOT
  * graph. It formats the nodes and edges of the tableau, including labels for
  * formulas and elimination rules. Authors:
  *   - Leslie Lucynda TINGUE
  *     - Bruno SANDELE
  *   - Dave Ronic DONKENG
  *
  * version 1.0
  */
package project

import project.{Tree, Status, Formula, EliminationRule}
import project.Formula.*
import project.EliminationRule.FormulaToEliminate

object TableauVisualizer:

  // ----- PRETTY PRINT Pour les formules -----

  private def prettyFormula(f: Formula): String =
    def wrap(phi: Formula): String = phi match
      case Prop(_) | Not(Prop(_)) => prettyFormula(phi)
      case _                      => s"(${prettyFormula(phi)})"

    f match
      case Prop(name)    => name
      case Not(phi)      => s"¬${wrap(phi)}"
      case And(p, q)     => s"${wrap(p)} ∧ ${wrap(q)}"
      case Or(p, q)      => s"${wrap(p)} ∨ ${wrap(q)}"
      case Implies(p, q) => s"${wrap(p)} → ${wrap(q)}"
      // bonus cases
      case Equivalent(p, q) => s"${wrap(p)} ↔ ${wrap(q)}"
      case Xor(p, q)        => s"${wrap(p)} ⊕ ${wrap(q)}"
      case Nand(p, q)       => s"¬(${wrap(p)} ∧ ${wrap(q)})"
      case Xnor(p, q)       => s"¬(${wrap(p)} ⊕ ${wrap(q)})"
      // n-ary operators
      case AndN(p) => p.map(wrap).mkString(" ∧ ")
      case OrN(p)  => p.map(wrap).mkString(" ∨ ")

  private def labelFormulas(fs: Set[Formula]): String =
    fs.map(prettyFormula).mkString(", ")

  private def ruleName(rule: EliminationRule): String = rule match
    case EliminationRule.OrRule         => "∨-rule"
    case EliminationRule.AndRule        => "∧-rule"
    case EliminationRule.ImpliesRule    => "→-rule"
    case EliminationRule.NotOrRule      => "¬∨-rule"
    case EliminationRule.NotAndRule     => "¬∧-rule"
    case EliminationRule.NotImpliesRule => "¬→-rule"
    case EliminationRule.DoubleNegation => "¬¬-rule"
    // Bonus
    case EliminationRule.EquivRule => "↔-rule"
    case EliminationRule.XorRule   => "⊕-rule"
    case EliminationRule.NandRule  => "NAND-rule"
    case EliminationRule.XnorRule  => "XNOR-rule"

    // n-ary operators
    case EliminationRule.AndNRule => "∧ⁿ-rule"
    case EliminationRule.OrNRule  => "∨ⁿ-rule"

  def viz(tree: Tree[Set[Formula]]): String =

    val header =
      """digraph Tableau {
        |  rankdir=TB;
        |  node [fontname="Helvetica", fontsize=14, shape=ellipse, style=rounded];
        |""".stripMargin

    def loop(t: Tree[Set[Formula]], path: List[Int]): String =
      val id = "T" + path.mkString(".")

      t match
        // ----- LEAF -----
        case Tree.Leaf(fs, status) =>
          val color = status match
            case Status.Open   => "green"
            case Status.Closed => "red"

          val label = s"""$id\\n${labelFormulas(fs)}"""

          s"""  "$id" [label="$label", color=$color, style=bold];"""

        // ----- BRANCH -----
        case Tree.Branch(fs, children) =>
          val label = s"""$id\\n${labelFormulas(fs)}"""
          val nodeDef = s"""  "$id" [label="$label"];"""

          val ruleLabel =
            FormulaToEliminate(fs) match
              case Some(f) =>
                val (rule, _) = EliminationRule.applyEliminationRule(f, fs - f)
                ruleName(rule)
              case None => ""

          val childrenDefs =
            children.zipWithIndex
              .map { case (child, index) =>
                val childPath = path :+ (index + 1)
                val childId = "T" + childPath.mkString(".")
                val childDot = loop(child, childPath)
                val edge = s"""  "$id" -> "$childId" [label="$ruleLabel"];"""

                s"$childDot\n$edge"
              }
              .mkString("\n")

          s"$nodeDef\n$childrenDefs"

    val body = loop(tree, List(1))

    header + body + "\n}"
