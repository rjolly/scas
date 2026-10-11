package scas.residue.binary

import scas.structure.BooleanRing
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.ufd.binary.Polynomial
import scas.power.compact.{BinaryPowerProduct, Lexicographic}
import scas.util.{Conversion, unary_~}
import scas.variable.Variable
import scas.base.Boolean

open class BooleanAlgebra(using Polynomial[Element[Boolean, Array[Int]], Boolean]) extends BooleanAlgebra.Impl {
  def this(pp: BinaryPowerProduct) = this(using new scas.polynomial.tree.binary.mutable.Polynomial(using Boolean, pp))
  def this(variables: Variable*) = this(Lexicographic.binary(variables*))
}

object BooleanAlgebra {
  def apply[S : Conversion[Variable]](s: S*) = new Conv(s.map(~_)*)

  trait Impl extends Residue[Element[Boolean, Array[Int]], Boolean] with scas.residue.BooleanAlgebra.Impl {
    extension (x: Element[Boolean, Array[Int]]) {
      override def defining = ring.defining(x)
    }
  }

  class Conv(variables: Variable*) extends BooleanAlgebra(variables*) with UniqueFactorizationDomain.Conv[Element[Boolean, Array[Int]]] with BooleanRing.Conv[Element[Boolean, Array[Int]]] {
    given instance: Conv = this
  }
}
