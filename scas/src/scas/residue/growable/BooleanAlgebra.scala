package scas.residue.growable

import scas.structure.BooleanRing
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.ufd.growable.PolynomialWithGB
import scas.power.growable.Lexicographic
import scas.variable.Variable
import scas.util.{Conversion, unary_~}
import scas.base.{BigInteger, Boolean}
import BigInteger.given

open class BooleanAlgebra(using PolynomialWithGB[Element[Boolean, Array[Int]], Boolean, Int]) extends scas.residue.BooleanAlgebra.Impl {
  def this(variables: Variable*) = this(using new scas.polynomial.tree.growable.PolynomialWithGB(using Boolean, new Lexicographic[Int](variables*)))
}

object BooleanAlgebra {
  def apply[S : Conversion[Variable]](s: S*) = new Conv(s.map(~_)*)

  class Conv(variables: Variable*) extends BooleanAlgebra(variables*) with UniqueFactorizationDomain.Conv[Element[Boolean, Array[Int]]] with BooleanRing.Conv[Element[Boolean, Array[Int]]] {
    given instance: Conv = this
  }
}
