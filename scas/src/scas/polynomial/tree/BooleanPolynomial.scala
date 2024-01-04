package scas.polynomial.tree

import scas.power.{PowerProduct, Lexicographic}
import scas.structure.commutative.conversion.BooleanUFD
import scas.structure.commutative.Field
import scas.polynomial.TreePolynomial
import TreePolynomial.Element
import scas.base.Boolean

class BooleanPolynomial[M](using Field[Boolean], PowerProduct[M]) extends TreePolynomial[Boolean, M] with scas.polynomial.BooleanPolynomial[Element[Boolean, M], M] with BooleanUFD[Element[Boolean, M]] {
  given instance: BooleanPolynomial[M] = this
}

object BooleanPolynomial {
  def apply(s: String*) = new BooleanPolynomial(using Boolean, Lexicographic[Int](s*))
}
