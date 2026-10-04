package scas.polynomial.tree

import scas.structure.commutative.Field
import scas.variable.Variable
import scas.power.splitable.{ArrayPowerProduct, Lexicographic}
import scas.polynomial.TreePolynomial.Element
import MultivariatePolynomialOverField.Impl

class MultivariatePolynomialOverField[C](using Field[C])(variables: Variable*) extends Impl[C] {
  override given pp: ArrayPowerProduct[Int] = new Lexicographic(variables*)
  def newInstance = [C] => (ring, pp) => new PolynomialWithSubresGCD(using ring)(pp.variables*)
}

object MultivariatePolynomialOverField {
  trait Impl[C] extends MultivariatePolynomial.Impl[C] with scas.polynomial.ufd.MultivariatePolynomialOverField[Element, C, Int]
}
