package scas.residue.binary

import scas.structure.{BooleanRing, Ring}
import scas.polynomial.binary.Polynomial
import scas.power.compact.binary.{PowerProduct, Lexicographic}
import scas.util.{Conversion, unary_~}
import scas.variable.Variable
import scas.base.Boolean
import BooleanAlgebra.Impl
import scas.residue.BooleanAlgebra.Element

abstract class BooleanAlgebra(using Polynomial[Element[Array[Int]], Boolean]) extends Impl with Ring.Conv[Element[Array[Int]]] with BooleanRing.Conv[Element[Array[Int]]] {
  def this(pp: PowerProduct) = this(using new scas.polynomial.tree.binary.mutable.Polynomial(using Boolean, pp))
  def this(variables: Variable*) = this(Lexicographic(variables*))
  def instance: BooleanAlgebra
}

object BooleanAlgebra {
  def apply(pp: PowerProduct) = new Conv1(pp)
  def apply[S : Conversion[Variable]](s: S*) = new Conv(s.map(~_)*)

  trait Impl extends scas.residue.BooleanAlgebra.Impl[Array[Int]] with Residue[Element[Array[Int]], Boolean] {
    extension (x: Element[Array[Int]]) {
      override def defining = ring.defining(x)
    }
  }

  class Conv1(pp: PowerProduct) extends BooleanAlgebra(pp) {
    given instance: BooleanAlgebra = this
  }

  class Conv(variables: Variable*) extends BooleanAlgebra(variables*) {
    given instance: BooleanAlgebra = this
  }
}
