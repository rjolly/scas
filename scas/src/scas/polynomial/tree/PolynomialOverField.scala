package scas.polynomial.tree

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.power.ArrayPowerProduct
import scas.structure.commutative.Field
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class PolynomialOverField[C, N](using val ring: Field[C], val pp: ArrayPowerProduct[N])(using ClassTag[N], Numeric[N]) extends TreePolynomial[C, Array[N]] with scas.polynomial.PolynomialOverFieldWithGB[Element[C, Array[N]], C, N] with scas.structure.commutative.conversion.UniqueFactorizationDomain[Element[C, Array[N]]] {
  given instance: PolynomialOverField[C, N] = this
  def newInstance(pp: ArrayPowerProduct[N]) = new PolynomialOverField(using ring, pp)
}
