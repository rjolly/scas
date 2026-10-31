package scas.power

import scas.variable.Variable
import POT.Element

open class POT[M : PowerProduct](using ArrayPowerProduct[Int]) extends Combined[M, Array[Int]] {
  def this(factory: PowerProduct[M])(variables: Variable*) = this(using factory, new Lexicographic[Int](variables*))
  def this(factory: PowerProduct[M])(name: String, dimension: Int) = this(factory)((for i <- 0 until dimension yield Variable(name, 0, i))*)
  override given factory2: ArrayPowerProduct[Int] = summon
  def apply(x: M) = (x, factory2.one)
  given pp2pot: (M => Element[M]) = this(_)
}

object POT {
  type Element[M] = (M, Array[Int])
}
