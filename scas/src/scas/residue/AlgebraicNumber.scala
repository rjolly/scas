package scas.residue

import scas.structure.commutative.Field
import scas.polynomial.tree.UnivariatePolynomial
import scas.polynomial.TreePolynomial.Element
import scas.power.Lexicographic
import scas.variable.Variable

trait AlgebraicNumber[C, M](using ring: UnivariatePolynomial[C, M]) extends Residue[Element[C, M], C, M] {
  export ring.coef2poly
}

object AlgebraicNumber {
  class Impl[C, M](using ring: UnivariatePolynomial[C, M]) extends conversion.AlgebraicNumber[C, M] {
    given instance: Impl[C, M] = this
  }

  def apply[C](ring: Field[C])(s: Variable*) = new Impl(using new UnivariatePolynomial(using ring, Lexicographic[Int](s: _*)))
}
