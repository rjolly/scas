package scas.polynomial.tree

import scas.power.splitable.Lexicographic
import scas.structure.commutative.{UniqueFactorizationDomain, Field}
import scas.util.Conversion
import scas.variable.Variable
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

trait MultivariatePolynomial[C, N] extends TreePolynomial[C, Array[N]] with scas.polynomial.ufd.MultivariatePolynomial[Element, C, N] with UniqueFactorizationDomain.Conv[Element[C, Array[N]]] {
  given instance: MultivariatePolynomial[C, N] = this
}

object MultivariatePolynomial {
  def withSimpleGCD[C, S : Conversion[Variable]](ring: UniqueFactorizationDomain[C])(s: S*) = new PolynomialWithSimpleGCD(using ring, Lexicographic(0)(s*))
  def withPrimitiveGCD[C, S : Conversion[Variable]](ring: UniqueFactorizationDomain[C])(s: S*) = new PolynomialWithPrimitiveGCD(using ring, Lexicographic(0)(s*))
  def withSubresGCD[C, S : Conversion[Variable]](ring: UniqueFactorizationDomain[C])(s: S*) = new PolynomialWithSubresGCD(using ring, Lexicographic(0)(s*))
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S*) = new MultivariatePolynomialOverField(using ring, Lexicographic(0)(s*))
}
