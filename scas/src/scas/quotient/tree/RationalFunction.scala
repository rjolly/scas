package scas.quotient.tree

import scas.power.impl.PowerProduct
import scas.structure.commutative.impl.Field
import scas.quotient.impl.TreeRationalFunction
import scas.polynomial.TreePolynomial.Element
import scas.power.Lexicographic

class RationalFunction[C : Field, M : PowerProduct] extends TreeRationalFunction[C, M] with scas.quotient.RationalFunction[Element[C, M], C, M] {
  given instance: RationalFunction[C, M] = this
}

object RationalFunction {
  def apply[C](ring: Field[C])(s: String*) = new RationalFunction(using ring, Lexicographic[Int](s: _*))
}
