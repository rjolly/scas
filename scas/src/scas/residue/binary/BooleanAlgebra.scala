package scas.residue.binary

import scas.structure.BooleanRing
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.ufd.binary.Polynomial
import scas.power.compact.Lexicographic
import scas.variable.Variable
import scas.util.{Conversion, unary_~}
import scas.base.Boolean

open class BooleanAlgebra(using Polynomial[Element[Boolean, Array[Int]], Boolean]) extends BooleanAlgebra.Impl {
  def this(variables: Variable*) = this(using new scas.polynomial.tree.binary.Polynomial(using Boolean, Lexicographic.binary(variables*)))
}

object BooleanAlgebra {
  def apply[S : Conversion[Variable]](s: S*) = new Conv(s.map(~_)*)

  trait Impl extends Residue[Element[Boolean, Array[Int]], Boolean] with scas.residue.BooleanAlgebra.Impl {
    override def init: Unit = {
      update((for (i <- 0 until ring.pp.variables.length) yield ring(i))*)
    }
  }

  class Conv(variables: Variable*) extends BooleanAlgebra(variables*) with UniqueFactorizationDomain.Conv[Element[Boolean, Array[Int]]] with BooleanRing.Conv[Element[Boolean, Array[Int]]] {
    given instance: Conv = this
  }
}
