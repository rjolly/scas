package scas.polynomial.tree

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.structure.commutative.{UniqueFactorizationDomain, Field}
import scas.variable.Variable
import scas.power.splitable.{ArrayPowerProduct, Lexicographic}
import MultivariatePolynomial.Element
import MultivariatePolynomialOverField.Impl

class MultivariatePolynomialOverField[C, N : {Numeric, ClassTag}](using Field[C], ArrayPowerProduct[N]) extends Impl[C, N] with UniqueFactorizationDomain.Conv[Element[N][C]] {
  def this(degree: N)(ring: Field[C])(variables: Variable*) = this(using summon, summon, ring, new Lexicographic(degree)(variables*))
  def newInstance = [C] => (ring, pp) => new PolynomialWithSubresGCD(pp.one.deg)(ring)(pp.variables*)
  given instance: MultivariatePolynomialOverField[C, N] = this
}

object MultivariatePolynomialOverField {
  trait Impl[C, N] extends MultivariatePolynomial.Impl[C, N] with scas.polynomial.ufd.MultivariatePolynomialOverField[Element[N], C, N]
}
