package scas.polynomial.tree

import scas.structure.Ring
import scas.power.{PowerProduct, Lexicographic}
import scas.polynomial.{TreePolynomial, WeylAlgebra}
import TreePolynomial.Element

class SolvablePolynomial[C : Ring, M : PowerProduct] extends TreePolynomial[C, M] with scas.polynomial.SolvablePolynomial[Element[C, M], C, M] with scas.structure.conversion.Ring[Element[C, M]] {
  given instance: SolvablePolynomial[C, M] = this
  def newInstance(pp: PowerProduct[M]) = new SolvablePolynomial(using ring, pp)
}

object SolvablePolynomial {
  def apply[C](ring: Ring[C])(s: String*) = new SolvablePolynomial(using ring, Lexicographic[Int](s*))
  def weylAlgebra[C](ring: Ring[C])(s: String*): SolvablePolynomial[C, Array[Int]] = new SolvablePolynomial(using ring, Lexicographic[Int](s*)) with WeylAlgebra[Element[C, Array[Int]], C, Array[Int]]
}
