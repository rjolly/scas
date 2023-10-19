package scas.residue.tree

import scas.power.impl.PowerProduct
import scas.structure.commutative.impl.Field
import scas.residue.impl.TreeAlgebraicNumber
import scas.polynomial.TreePolynomial.Element
import scas.power.Lexicographic
import scas.variable.Variable

class AlgebraicNumber[C : Field, M : PowerProduct] extends TreeAlgebraicNumber[C, M] with scas.residue.AlgebraicNumber[Element[C, M], C, M] {
  given instance: AlgebraicNumber[C, M] = this
}

object AlgebraicNumber {
  def apply[C](ring: Field[C])(s: Variable*) = new AlgebraicNumber(using ring, new Lexicographic[Int](s: _*))
}
