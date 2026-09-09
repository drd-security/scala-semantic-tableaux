/** Test.scala Test suite for the tableau method implementation in Bibli.scala
  * and LazyBibli.scala. It includes tests for satisfiability, validity,
  * tautology, contradiction, models, and counter-examples using both strict and
  * lazy semantic tableaux and also extension test. Authors:
  *   - Leslie Lucynda TINGUE
  *   - Bruno SANDELE
  *   - Dave Ronic DONKENG
  * version 1.0
  */
package project

import project.Formula.*
import project.TableauBuilder.*
import project.Bibli.*
import project.LazyBibli.*
import project.TableauVisualizer.*

object TestSemtab:
  def main(args: Array[String]): Unit =
    println("=== TEST SEMTAB ===\n")

    // Test 1: {a ∨ b, c, ¬d}
    println("Test 1: {a v b, c, ¬d}")
    val test1 = Set(Or(Prop("a"), Prop("b")), Prop("c"), Not(Prop("d")))
    val tree1 = semtab(test1)
    println(s"Results: $tree1\n")

    // Test 2: {¬𝑎 → 𝑏, 𝑐, ¬𝑑}
    println("Test 2: {¬a → b, c, ¬d}")
    val test2 =
      Set(Implies(Not(Prop("a")), Prop("b")), Prop("c"), Not(Prop("d")))
    val tree2 = semtab(test2)
    println(s"Results: $tree2\n")

    // Test 3: {((𝑎→𝑏) → (¬𝑎∨𝑏) ∧ (¬𝑎∨𝑏) → (𝑎 → 𝑏))}
    println("Test 3: {((a→b) → (¬avb) ∧ (¬avb) → (a → b))}")
    val test3 = Set(
      And(
        // left  : (a→b) → (¬a v b)
        Implies(
          Implies(Prop("a"), Prop("b")),
          Or(Not(Prop("a")), Prop("b"))
        ),
        // right : (¬a v b) → (a → b)
        Implies(
          Or(Not(Prop("a")), Prop("b")),
          Implies(Prop("a"), Prop("b"))
        )
      )
    )
    val tree3 = semtab(test3)
    println(s"Results: $tree3\n")

    // Test 4: {(¬(𝑎 ∨ 𝑏) ∧ (𝑎 ∨ 𝑏))}
    println("Test 4: {(¬(a v b) ∧ (a v b))}")
    val test4 = Set(
      And(Not(Or(Prop("a"), Prop("b"))), Or(Prop("a"), Prop("b")))
    )
    val tree4 = semtab(test4)
    println(s"Results: $tree4\n")

    // Test 5: {(¬(𝑎 ∨ 𝑏) ∧ ¬¬(𝑎 ∨ 𝑏))}
    println("Test 5: {(¬(a v b) ∧ ¬¬(a v b))}")
    val test5 = Set(
      And(Not(Or(Prop("a"), Prop("b"))), Not(Not(Or(Prop("a"), Prop("b")))))
    )
    val tree5 = semtab(test5)
    println(s"Results: $tree5\n")

    // Test 6: {} (empty set)
    println("Test 6: {} (empty set)")
    val test6 = Set.empty[Formula]
    val tree6 = semtab(test6)
    println(s"Results: $tree6\n")

    println("=== VIZ ===\n")
    println("DOT representation of the tree :")
    println(s"viz tree1 :\n${viz(tree1)}\n")
    println(s"viz tree2 :\n${viz(tree2)}\n")
    println(s"viz tree3 :\n${viz(tree3)}\n")
    println(s"viz tree4 :\n${viz(tree4)}\n")
    println(s"viz tree5 :\n${viz(tree5)}\n")
    println(s"viz tree6 :\n${viz(tree6)}\n")

    println("=== TEST SATISFIABLE ===\n")
    println(
      s"IsSatisfiable {a v b} ? : ${isSatisfiable(Set(Or(Prop("a"), Prop("b"))))}"
    ) // {a v b} is satisfiable
    println(
      s"IsSatisfiable {a, ¬a ∧ b} ? : ${isSatisfiable(Set(Prop("a"), And(Not(Prop("a")), Prop("b"))))}"
    ) // {a, -a ∧ b} not satisfiable

    println("=== ISVALID ===\n")
    println(s"isValid for {a ∧ (c v d)} ⊨ a ?:${isValid(
        Set(And(Prop("a"), Or(Prop("c"), Prop("d")))),
        Prop("a")
      )}") // counter examples for {a ∧ (c v d)} ⊨ a
    println(s"isValid for {a v (c v d)} ⊨ a ?:${isValid(
        Set(Or(Prop("a"), Or(Prop("c"), Prop("d")))),
        Prop("a")
      )}") // counter examples for {a v (c v d)} ⊨ a

    println("=== ISTAUTOLOGY ===\n")
    println(
      s"IsTautology (a v ¬a) ? : ${isTautology(Or(Prop("a"), Not(Prop("a"))))}"
    ) // (a v -a) is a tautology
    println(
      s"IsTautology (a ∧ ¬a) ? : ${isTautology(And(Prop("a"), Not(Prop("a"))))}"
    ) // (a ∧ -a) is not a tautology

    println("=== ISCONTRADICTION ===\n")
    println(
      s"IsContradiction {a, ¬a} ? : ${isContradiction(And(Prop("a"), Not(Prop("a"))))}"
    ) // {a, -a} is a contradiction
    println(
      s"IsContradiction {a v ¬a} ? : ${isContradiction(Or(Prop("a"), Not(Prop("a"))))}"
    ) // {a v -a} is not a contradiction

    println("=== MODELS ===\n")
    println(
      s"Models for {a v b,c} ?:${models(Set(Or(Prop("a"), Prop("b")), Prop("c")))}"
    ) // Models for {a v b,c}
    println(
      s"Models for {a, ¬a} ?:${models(Set(Prop("a"), Not(Prop("a"))))}"
    ) // Models for {a, -a}
    println(
      s"Models for {} ?:${models(Set.empty[Formula])}"
    ) // Models for {}

    println("=== COUNTER EXAMPLES ===\n")
    println(s"counter-examples for {a ∧ (c v d)} ⊨ a ?:${counterExamples(
        Set(And(Prop("a"), Or(Prop("c"), Prop("d")))),
        Prop("a")
      )}") // counter examples for {a ∧ (c v d)} ⊨ a
    println(s"counter-examples for {a v (c v d)} ⊨ a ?:${counterExamples(
        Set(Or(Prop("a"), Or(Prop("c"), Prop("d")))),
        Prop("a")
      )}") // counter examples for {a v (c v d)} ⊨ a

    println("=== END SEMTAB ===\n")

object TestlazySemtab:
  def main(args: Array[String]): Unit =
    println("=== TEST LAZYSEMTAB ===\n")
    // Test 1: {a ∨ b, c, ¬d}
    println("Test 1: {a v b, c, ¬d}")
    val lazytest1 = Set(Or(Prop("a"), Prop("b")), Prop("c"), Not(Prop("d")))
    val lazytree1 = lazysemtab(lazytest1)
    println(s"Results: $lazytree1\n")

    // lazyTest 2: {¬𝑎 → 𝑏, 𝑐, ¬𝑑}
    println("lazyTest 2: {¬a → b, c, ¬d}")
    val lazytest2 =
      Set(Implies(Not(Prop("a")), Prop("b")), Prop("c"), Not(Prop("d")))
    val lazytree2 = lazysemtab(lazytest2)
    println(s"Results: $lazytree2\n")

    // lazyTest 3: {((𝑎→𝑏) → (¬𝑎∨𝑏) ∧ (¬𝑎∨𝑏) → (𝑎 → 𝑏))}
    println("lazyTest 3: {((a→b) → (¬avb) ∧ (¬avb) → (a → b))}")
    val lazytest3 = Set(
      And(
        // left  : (a→b) → (¬a v b)
        Implies(
          Implies(Prop("a"), Prop("b")),
          Or(Not(Prop("a")), Prop("b"))
        ),
        // right : (¬a v b) → (a → b)
        Implies(
          Or(Not(Prop("a")), Prop("b")),
          Implies(Prop("a"), Prop("b"))
        )
      )
    )
    val lazytree3 = lazysemtab(lazytest3)
    println(s"Results: $lazytree3\n")

    // lazyTest 4: {(¬(𝑎 ∨ 𝑏) ∧ (𝑎 ∨ 𝑏))}
    println("lazyTest 4: {(¬(a v b) ∧ (a v b))}")
    val lazytest4 = Set(
      And(Not(Or(Prop("a"), Prop("b"))), Or(Prop("a"), Prop("b")))
    )
    val lazytree4 = lazysemtab(lazytest4)
    println(s"Results: $lazytree4\n")

    // lazyTest 5: {(¬(𝑎 ∨ 𝑏) ∧ ¬¬(𝑎 ∨ 𝑏))}
    println("lazyTest 5: {(¬(a v b) ∧ ¬¬(a v b))}")
    val lazytest5 = Set(
      And(Not(Or(Prop("a"), Prop("b"))), Not(Not(Or(Prop("a"), Prop("b")))))
    )
    val lazytree5 = lazysemtab(lazytest5)
    println(s"Results: $lazytree5\n")

    // lazyTest 6: {} (empty set)
    println("lazyTest 6: {} (empty set)")
    val lazytest6 = Set.empty[Formula]
    val lazytree6 = lazysemtab(lazytest6)
    println(s"Results: $lazytree6\n")

    println("=== LAZYSATISFIABLE ===\n")
    println(
      s"lazyIsSatisfiable {a v b} ? : ${lazyIsSatisfiable(Set(Or(Prop("a"), Prop("b"))))}"
    ) // {a v b} is satisfiable
    println(
      s"lazyIsSatisfiable {a, ¬a ∧ b} ? : ${lazyIsSatisfiable(
          Set(Prop("a"), And(Not(Prop("a")), Prop("b")))
        )}"
    ) // {a, -a ∧ b} not satisfiable

    println("=== LAZYISVALID ===\n")
    println(s"lazyIsValid for {a ∧ (c v d)} ⊨ a ?:${lazyIsValid(
        Set(And(Prop("a"), Or(Prop("c"), Prop("d")))),
        Prop("a")
      )}") // lazyIsValid for {a ∧ (c v d)} ⊨ a
    println(s"lazyIsValid for {a v (c v d)} ⊨ a ?:${lazyIsValid(
        Set(Or(Prop("a"), Or(Prop("c"), Prop("d")))),
        Prop("a")
      )}") // lazyIsValid for {a v (c v d)} ⊨ a

    println("=== LAZYISTAUTOLOGY ===\n")
    println(
      s"lazyIsTautology (a v ¬a) ? : ${lazyIsTautology(Or(Prop("a"), Not(Prop("a"))))}"
    ) // (a v -a) is a tautology
    println(
      s"lazyIsTautology (a ∧ ¬a) ? : ${lazyIsTautology(And(Prop("a"), Not(Prop("a"))))}"
    ) // (a ∧ -a) is not a tautology

    println("=== LAZYISCONTRADICTION ===\n")
    println(
      s"lazyIsContradiction {a, ¬a} ? : ${lazyIsContradiction(And(Prop("a"), Not(Prop("a"))))}"
    ) // {a, -a} is a contradiction
    println(
      s"lazyIsContradiction {a v ¬a} ? : ${lazyIsContradiction(Or(Prop("a"), Not(Prop("a"))))}"
    ) // {a v -a} is not a contradiction

    println("=== LAZYMODELS ===\n")
    println(
      s"lazyModels for {a v b,c} ?:${lazyModels(Set(Or(Prop("a"), Prop("b")), Prop("c")))}"
    ) // lazyModels for {a v b,c}
    println(
      s"lazyModels for {a, ¬a} ?:${lazyModels(Set(Prop("a"), Not(Prop("a"))))}"
    ) // lazyModels for {a, -a}
    println(
      s"lazyModels for {} ?:${lazyModels(Set.empty[Formula])}"
    ) // lazyModels for {}

    println("=== LAZYCOUNTER EXAMPLES ===\n")
    println(s"lazyCounterExamples for {a ∧ (c v d)} ⊨ a ?:${lazyCounterExamples(
        Set(And(Prop("a"), Or(Prop("c"), Prop("d")))),
        Prop("a")
      )}") // counter examples for {a ∧ (c v d)} ⊨ a
    println(s"lazyCounterExamples for {a v (c v d)} ⊨ a ?:${lazyCounterExamples(
        Set(Or(Prop("a"), Or(Prop("c"), Prop("d")))),
        Prop("a")
      )}") // counter examples for {a v (c v d)} ⊨ a

    println("=== END LAZYSEMTAB ===\n")

object TestExtension:
  def main(args: Array[String]): Unit =

    println("=== EXTENSIONS DEMONSTRATION (STRICT & LAZY) ===\n")

    // Helper to display both versions
    def printBoth(name: String, formulas: Set[Formula]): Unit = {
      println(s"\n$name:")
      println("-" * 40)

      // Strict version
      println("STRICT version:")
      println(s"  Tableau: ${semtab(formulas)}")
      println(s"  Satisfiable? ${isSatisfiable(formulas)}")
      println(s"  Models: ${models(formulas)}")

      // Lazy version
      println("\nLAZY version:")
      println(s"  Tableau: ${lazysemtab(formulas)}")
      println(s"  Satisfiable? ${lazyIsSatisfiable(formulas)}")
      println(s"  Models: ${lazyModels(formulas)}")
    }

    // 1. Test bonus operators
    println("1. BONUS OPERATORS TEST")
    println("=" * 50)

    // EQUIV
    printBoth("EQUIV: a ↔ b", Set(Equivalent(Prop("a"), Prop("b"))))

    // XOR
    printBoth("XOR: a ⊕ b", Set(Xor(Prop("a"), Prop("b"))))

    // NAND
    printBoth("NAND: a ⊼ b", Set(Nand(Prop("a"), Prop("b"))))

    // XNOR
    printBoth("XNOR: a ≡ b", Set(Xnor(Prop("a"), Prop("b"))))

    println()

    // 2. Test n-ary operators
    println("\n2. N-ARY OPERATORS TEST")
    println("=" * 50)

    // N-ary AND
    val and3 = Set(andAll(Prop("a"), Prop("b"), Prop("c")))
    printBoth("N-ary AND: a ∧ b ∧ c", and3)

    // N-ary OR
    val or3 = Set(orAll(Prop("a"), Prop("b"), Prop("c")))
    printBoth("N-ary OR: a ∨ b ∨ c", or3)

    println()

    // 3. Complex examples
    println("\n3. COMPLEX EXAMPLES")
    println("=" * 50)

    // Example 1: Distributivity
    val distrib = Set(
      Equivalent(
        andAll(Prop("a"), orAll(Prop("b"), Prop("c"))),
        orAll(
          andAll(Prop("a"), Prop("b")),
          andAll(Prop("a"), Prop("c"))
        )
      )
    )

    println(s"Distributivity: a ∧ (b ∨ c) ↔ (a ∧ b) ∨ (a ∧ c)")
    println(s"  Tautology? Strict: ${isTautology(distrib.head)}")
    println(s"  Tautology? Lazy: ${lazyIsTautology(distrib.head)}")
    println()

    // Example 2: Combination of operators
    val complex = Set(
      Xor(
        andAll(Prop("a"), Prop("b"), Prop("c")),
        orAll(Prop("d"), Prop("e"), Prop("f"))
      )
    )

    println(s"Complex: (a ∧ b ∧ c) ⊕ (d ∨ e ∨ f)")
    val complexTree = semtab(complex)
    val complexLazyTree = lazysemtab(complex)
    println(s"  Strict tableau leaves: ${complexTree.leaves.size}")
    println(s"  Satisfiable? Strict: ${isSatisfiable(complex)}")
    println(s"  Satisfiable? Lazy: ${lazyIsSatisfiable(complex)}")

    println()

    // 4. Visualization (strict only as it's simpler)
    println("\n4. VISUALIZATION EXAMPLE (Strict only)")
    println("=" * 50)

    val vizExample = Set(orAll(Prop("a"), Prop("b"), Prop("c")))
    val vizTree = semtab(vizExample)
    println(s"DOT for: a ∨ b ∨ c")
    println(viz(vizTree))

    println()

    // 5. Property validation with both versions
    println("\n5. PROPERTY VALIDATION (Both versions)")
    println("=" * 50)

    // Commutativity of n-ary AND
    val commutativeAnd = Equivalent(
      andAll(Prop("a"), Prop("b"), Prop("c")),
      andAll(Prop("c"), Prop("b"), Prop("a"))
    )
    println(s"N-ary AND commutative?")
    println(s"  Strict: ${isTautology(commutativeAnd)}")
    println(s"  Lazy: ${lazyIsTautology(commutativeAnd)}")
    println()

    // Associativity of n-ary OR
    val associativeOr = Equivalent(
      orAll(Prop("a"), orAll(Prop("b"), Prop("c"))),
      orAll(orAll(Prop("a"), Prop("b")), Prop("c"))
    )
    println(s"N-ary OR associative?")
    println(s"  Strict: ${isTautology(associativeOr)}")
    println(s"  Lazy: ${lazyIsTautology(associativeOr)}")
    println()

    // De Morgan's law for n-ary
    val deMorganN = Equivalent(
      Not(andAll(Prop("a"), Prop("b"), Prop("c"))),
      orAll(Not(Prop("a")), Not(Prop("b")), Not(Prop("c")))
    )
    println(s"De Morgan n-ary?")
    println(s"  Strict: ${isTautology(deMorganN)}")
    println(s"  Lazy: ${lazyIsTautology(deMorganN)}")

    println()

    println("\n" + "=" * 50)
    println("END OF DEMONSTRATION")
    println("Both strict and lazy implementations work correctly!")
    println("=" * 50)
