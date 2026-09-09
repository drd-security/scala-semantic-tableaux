/** Tableau.scala Defines the tableau tree structure and status for
  * propositional logic formulas. Authors:
  *   - Leslie Lucynda TINGUE
  *   - Bruno SANDELE
  *   - Dave Ronic DONKENG
  * version 1.0
  */
package project

import project.Formula

/** Model represents a partial or total valuation of propositional variables.
  * Each variable maps to a Boolean value (true or false).
  */
type Model = Map[String, Boolean]

/** Status of a tableau:
  *   - Open : the tableau is open and represents a set of models.
  *   - Closed: the tableau is closed (contradiction).
  */
enum Status:
  case Open
  case Closed

/** Tableau tree.
  *
  *   - Leaf : a tableau node with no children (open or closed).
  *   - Branch : a tableau node resulting from eliminating a formula, containing
  *     the resulting list of child subtrees.
  */
enum Tree[+A]:
  case Leaf(value: A, status: Status)
  case Branch(value: A, children: List[Tree[A]])

  /** Returns a list of all leaf values in the tree (DFS). */
  def leaves: List[A] = this match
    case Leaf(v, _)    => List(v)
    case Branch(_, ch) => ch.flatMap(_.leaves)

/** A fully lazy tree structure for tableaux.
  *
  * Both formulas and children are lazy and evaluated only when needed.
  */
enum LazyTree[+A]:
  case Leaf(value: () => A, status: () => Status)
  case Branch(value: () => A, children: () => LazyList[() => LazyTree[A]])

  /** Lazy depth-first (DFS) traversal of the tree. Returns a LazyList of all
    * nodes. Nodes are evaluated only as needed.
    */
  def toLazyList: LazyList[A] =
    def iter(queue: LazyList[() => LazyTree[A]]): LazyList[A] =
      queue match
        case LazyList() => LazyList.empty
        case th #:: rest =>
          th() match
            case Leaf(value, _) =>
              value() #:: iter(rest)
            case Branch(value, children) =>
              value() #:: iter(children() #::: rest)

    iter(LazyList(() => this))

  /** Returns a LazyList of values for **closed leaves** only (DFS). */
  def closedLeavesLazyList: LazyList[A] =
    def iter(queue: LazyList[() => LazyTree[A]]): LazyList[A] =
      queue match
        case LazyList() => LazyList.empty
        case th #:: rest =>
          th() match
            case Leaf(value, status) =>
              if status() == Status.Closed then value() #:: iter(rest)
              else iter(rest)
            case Branch(_, children) =>
              iter(children() #::: rest)

    iter(LazyList(() => this))

  /** Returns a LazyList of values for **open leaves** only (DFS). */
  def openLeavesLazyList: LazyList[A] =
    def iter(queue: LazyList[() => LazyTree[A]]): LazyList[A] =
      queue match
        case LazyList() => LazyList.empty
        case th #:: rest =>
          th() match
            case Leaf(value, status) =>
              if status() == Status.Open then value() #:: iter(rest)
              else iter(rest)
            case Branch(_, children) =>
              iter(children() #::: rest)

    iter(LazyList(() => this))
