package scas.residue.growable

import scas.polynomial.TreePolynomial.Element
import scas.polynomial.ufd.growable.PolynomialWithGB
import scas.power.growable.Lexicographic
import scas.variable.Variable
import scas.base.Boolean

open class BooleanAlgebra(using PolynomialWithGB[Element[Boolean, Array[Int]], Boolean, Int]) extends Residue[Element[Boolean, Array[Int]], Boolean, Int] with scas.residue.BooleanAlgebra.Impl {
  def this(variables: Variable*) = this(using new scas.polynomial.tree.growable.PolynomialWithGB(using Boolean, new Lexicographic[Int](variables*)))
}
