package scas.polynomial.ufd.tree.int

import scas.polynomial.ordering.Lexicographic
import scas.polynomial.PowerProduct
import scas.structure.Field
import scas.ordering

object UnivariatePolynomial {
  def apply[C](ring: Field[C], name: String) = scas.polynomial.ufd.tree.UnivariatePolynomial(ring, PowerProduct[Int](name))
}
