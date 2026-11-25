package scas.power

import scas.power.sparse.Lexicographic
import scas.variable.Variable
import POT.Impl

open class POT[M : PowerProduct](using PowerProduct[Array[Int]]) extends Impl[M] {
  def this(factory: PowerProduct[M])(variables: Variable*) = this(using factory, new Lexicographic(0)(variables*))
  def this(factory: PowerProduct[M])(name: String, dimension: Int) = this(factory)((for i <- 0 until dimension yield Variable(name, 0, i))*)
}

object POT {
  trait Impl[M] extends Combined[M, Array[Int]] {
    def apply(x: M) = (x, factory2.one)
    given pp2pot: (M => Element[M]) = this(_)
  }

  type Element[M] = (M, Array[Int])
}
