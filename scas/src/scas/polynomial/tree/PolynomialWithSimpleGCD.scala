package scas.polynomial.tree

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.structure.commutative.UniqueFactorizationDomain
import scas.variable.Variable
import MultivariatePolynomial.Element

class PolynomialWithSimpleGCD[C, N : {Numeric, ClassTag}](degree: N)(ring: UniqueFactorizationDomain[C])(variables: Variable*) extends MultivariatePolynomial(degree)(ring)(variables*) with scas.polynomial.ufd.PolynomialWithSimpleGCD[Element[N], C, N] {
  def newInstance = [C] => (ring, pp) => new PolynomialWithSimpleGCD(degree)(ring)(pp.variables*)
}
