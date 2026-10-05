package scas.residue.binary

import scas.polynomial.TreePolynomial.Element
import scas.polynomial.binary.Polynomial
import scas.power.compact.Lexicographic
import scas.variable.Variable
import scas.base.{BigInteger, Boolean}
import BigInteger.given

open class BooleanAlgebra(using Polynomial[Element[Boolean, Array[Int]], Boolean]) extends BooleanAlgebra.Impl {
  def this(variables: Variable*) = this(using new scas.polynomial.tree.binary.Polynomial(using Boolean, Lexicographic.binary(variables*)))
}

object BooleanAlgebra {
  trait Impl extends Residue[Element[Boolean, Array[Int]], Boolean] with scas.residue.BooleanAlgebra.Impl {
    override def init: Unit = {
      update((for (i <- 0 until ring.pp.variables.length) yield ring(i))*)
    }
  }
}
