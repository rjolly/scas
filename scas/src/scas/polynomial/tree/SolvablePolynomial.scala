package scas.polynomial.tree

import scas.structure.impl.Ring
import scas.power.{PowerProduct, Lexicographic}
import scas.polynomial.conversion.Polynomial
import scas.polynomial.{TreePolynomial, WeylAlgebra}
import TreePolynomial.Element

class SolvablePolynomial[C : Ring, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.SolvablePolynomial[Element[C, M], C, M] with Polynomial[Element[C, M], C, M] {
  given instance: SolvablePolynomial[C, M] = this
}

object SolvablePolynomial {
  def apply[C](ring: Ring[C])(s: String*) = new SolvablePolynomial(using ring, Lexicographic[Int](s: _*))
  def weylAlgebra[C](ring: Ring[C])(s: String*) = new SolvablePolynomial(using ring, Lexicographic[Int](s: _*)) with WeylAlgebra[Element[C, Array[Int]], C, Array[Int]]
}
