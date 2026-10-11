package scas.polynomial.tree

import scas.structure.commutative.{UniqueFactorizationDomain, Field}
import scas.variable.Variable
import scas.power.splitable.{ArrayPowerProduct, Lexicographic}
import scas.polynomial.TreePolynomial.Element
import MultivariatePolynomialOverField.Impl

class MultivariatePolynomialOverField[C](using Field[C], ArrayPowerProduct[Int]) extends Impl[C, Int] with UniqueFactorizationDomain.Conv[Element[C, Array[Int]]] {
  def this(ring: Field[C])(variables: Variable*) = this(using ring, new Lexicographic(variables*))
  def newInstance = [C] => (ring, pp) => new PolynomialWithSubresGCD(ring)(pp.variables*)
  given instance: MultivariatePolynomialOverField[C] = this
}

object MultivariatePolynomialOverField {
  trait Impl[C, N] extends MultivariatePolynomial.Impl[C, N] with scas.polynomial.ufd.MultivariatePolynomialOverField[Element, C, N]
}
