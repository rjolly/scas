package scas.polynomial.tree

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.structure.commutative.UniqueFactorizationDomain
import scas.variable.Variable
import MultivariatePolynomial.Element

class PolynomialWithSubresGCD[C, N : {Numeric, ClassTag}](degree: N)(ring: UniqueFactorizationDomain[C])(variables: Variable*) extends MultivariatePolynomial(degree)(ring)(variables*) with scas.polynomial.ufd.PolynomialWithSubresGCD[Element[N], C, N] {
  def newInstance = [C] => (ring, pp) => new PolynomialWithSubresGCD(degree)(ring)(pp.variables*)
}
