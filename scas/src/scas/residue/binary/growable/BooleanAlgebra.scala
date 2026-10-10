package scas.residue.binary.growable

import scas.polynomial.TreePolynomial.Element
import scas.polynomial.ufd.binary.growable.Polynomial
import scas.power.compact.growable.Lexicographic
import scas.variable.Variable
import scas.base.Boolean

open class BooleanAlgebra(using Polynomial[Element[Boolean, Array[Int]], Boolean]) extends BooleanAlgebra.Impl {
  def this(variables: Variable*) = this(using new scas.polynomial.tree.binary.growable.Polynomial(using Boolean, Lexicographic.binary(variables*)))
}

object BooleanAlgebra {
  trait Impl extends Residue[Element[Boolean, Array[Int]], Boolean] with scas.residue.binary.BooleanAlgebra.Impl with scas.residue.growable.BooleanAlgebra.Impl
}
