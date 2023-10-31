package scas.polynomial.tree

import scas.structure.impl.Ring
import scas.power.impl.PowerProduct
import scas.polynomial.Polynomial
import scas.polynomial.impl.{TreePolynomial, WeylAlgebra}
import scas.polynomial.TreePolynomial.Element
import scas.power.Lexicographic

class SolvablePolynomial[C : Ring, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.impl.SolvablePolynomial[Element[C, M], C, M] with Polynomial[Element[C, M], C, M] {
  given instance: SolvablePolynomial[C, M] = this
}

object SolvablePolynomial {
  def apply[C](ring: Ring[C])(s: String*) = new SolvablePolynomial(using ring, Lexicographic(0)(s: _*))
  def weylAlgebra[C](ring: Ring[C])(s: String*) = new SolvablePolynomial(using ring, Lexicographic(0)(s: _*)) with WeylAlgebra[Element[C, Array[Int]], C, Array[Int]]
}
