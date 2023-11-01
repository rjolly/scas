package scas.residue

import scas.structure.commutative.impl.Field
import scas.polynomial.tree.UnivariatePolynomial
import scas.polynomial.TreePolynomial.Element
import scas.power.Lexicographic
import scas.variable.Variable

trait AlgebraicNumber[C, M] extends impl.AlgebraicNumber[C, M] with Residue[Element[C, M], C, M] {
  given instance: AlgebraicNumber[C, M]
}

object AlgebraicNumber {
  def apply[C](ring: Field[C])(s: Variable*) = {
    given UnivariatePolynomial[C, Array[Int]] = new UnivariatePolynomial(using ring, Lexicographic[Int](s: _*))
    new AlgebraicNumber[C, Array[Int]] {
      given instance: AlgebraicNumber[C, Array[Int]] = this
    }
  }
}
