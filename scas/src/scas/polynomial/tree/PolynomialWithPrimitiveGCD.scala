package scas.polynomial.tree

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.structure.commutative.UniqueFactorizationDomain
import scas.variable.Variable
import scas.polynomial.TreePolynomial.Element

class PolynomialWithPrimitiveGCD[C, N : {Numeric, ClassTag}](degree: N)(ring: UniqueFactorizationDomain[C])(variables: Variable*) extends MultivariatePolynomial(degree)(ring)(variables*) with scas.polynomial.ufd.PolynomialWithPrimitiveGCD[Element, C, N] {
  def newInstance = [C] => (ring, pp) => new PolynomialWithPrimitiveGCD(degree)(ring)(pp.variables*)
}
