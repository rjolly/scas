package scas.polynomial.tree

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.structure.commutative.UniqueFactorizationDomain
import scas.variable.Variable
import scas.power.splitable.{ArrayPowerProduct, Lexicographic}
import scas.polynomial.TreePolynomial.Element

class PolynomialWithSubresGCD[C, N : {Numeric, ClassTag}](using UniqueFactorizationDomain[C], ArrayPowerProduct[N]) extends MultivariatePolynomial[C, N] with scas.polynomial.ufd.PolynomialWithSubresGCD[Element, C, N] {
  def this(degree: N)(ring: UniqueFactorizationDomain[C])(variables: Variable*) = this(using summon, summon, ring, new Lexicographic(degree)(variables*))
  def newInstance = [C] => (ring, pp) => new PolynomialWithSubresGCD(using summon, summon, ring, pp)
}
