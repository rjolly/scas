package scas.power

import scala.compiletime.deferred
import scas.variable.Variable

open class POT[M : PowerProduct](using ArrayPowerProduct[Int]) extends Combined[M, Array[Int]] {
  def this(factory: PowerProduct[M])(variables: Variable*) = this(using factory, new Lexicographic[Int](variables*))
  def this(factory: PowerProduct[M])(name: String, dimension: Int) = this(factory)((for i <- 0 until dimension yield Variable(name, 0, i))*)
  override given factory2: ArrayPowerProduct[Int] = summon
}

object POT {
  type Element[M] = (M, Array[Int])
}
