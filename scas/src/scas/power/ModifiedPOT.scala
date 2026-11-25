package scas.power

import scala.compiletime.deferred
import scas.variable.Variable
import ModifiedPOT.Impl

class ModifiedPOT[M : PowerProduct](using ArrayPowerProduct[Int]) extends Impl[M] {
  def this(factory: PowerProduct[M])(variables: Variable*) = this(using factory, new Lexicographic(0)(variables*))
  def this(factory: PowerProduct[M])(name: String, dimension: Int) = this(factory)((for i <- 0 until dimension yield Variable(name, 0, i))*)
}

object ModifiedPOT {
  trait Impl[M] extends POT.Impl[M] {
    given factory2: ArrayPowerProduct[Int] = deferred
    override def compare(x: (M, Array[Int]), y: (M, Array[Int])) = {
      val (a, b) = x
      val (c, d) = y
      var i = 0
      if b.get(i) < d.get(i) then return -1
      if b.get(i) > d.get(i) then return 1
      val s = factory1.compare(a, c)
      if s < 0 then return -1
      if s > 0 then return 1
      i += 1
      while i < factory2.nbvars do {
        if b.get(i) < d.get(i) then return -1
        if b.get(i) > d.get(i) then return 1
        i += 1
      }
      return 0
    }
  }
}
