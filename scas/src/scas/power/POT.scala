package scas.power

import scala.compiletime.deferred
import scas.variable.Variable
import POT.Impl

open class POT[M : PowerProduct](using ArrayPowerProduct[Int]) extends Impl[M] {
  def this(factory: PowerProduct[M])(variables: Variable*) = this(using factory, new Lexicographic[Int](variables*))
  def this(factory: PowerProduct[M])(name: String, dimension: Int) = this(factory)((for i <- 0 until dimension yield Variable(name, 0, i))*)
}

object POT {
  trait Impl[M] extends Combined[M, Array[Int]] {
    given factory2: ArrayPowerProduct[Int] = deferred
  }
}
