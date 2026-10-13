package scas.residue.binary

import scas.structure.{BooleanRing, Ring}
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.binary.Polynomial
import scas.power.compact.{BinaryPowerProduct, Lexicographic}
import scas.util.{Conversion, unary_~}
import scas.variable.Variable
import scas.base.Boolean
import BooleanAlgebra.Impl

abstract class BooleanAlgebra(using Polynomial[Element[Boolean, Array[Int]], Boolean]) extends Impl with Ring.Conv[Element[Boolean, Array[Int]]] with BooleanRing.Conv[Element[Boolean, Array[Int]]] {
  def this(pp: BinaryPowerProduct) = this(using new scas.polynomial.tree.binary.mutable.Polynomial(using Boolean, pp))
  def this(variables: Variable*) = this(Lexicographic.binary(variables*))
  def instance: BooleanAlgebra
}

object BooleanAlgebra {
  def apply[S : Conversion[Variable]](s: S*) = new Conv(s.map(~_)*)

  trait Impl extends scas.residue.BooleanAlgebra.Impl[Array[Int]] with Residue[Element[Boolean, Array[Int]], Boolean] {
    extension (x: Element[Boolean, Array[Int]]) {
      override def defining = ring.defining(x)
    }
  }

  class Conv(variables: Variable*) extends BooleanAlgebra(variables*) {
    given instance: BooleanAlgebra = this
  }
}
