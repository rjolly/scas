package scas.polynomial.tree

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.power.ArrayPowerProduct
import scas.structure.commutative.{Field, UniqueFactorizationDomain}
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class PolynomialOverFieldWithGB[C, N](using val ring: Field[C], val pp: ArrayPowerProduct[N])(using ClassTag[N], Numeric[N]) extends TreePolynomial[C, Array[N]] with scas.polynomial.ufd.PolynomialOverFieldWithGB[Element[C, Array[N]], C, N] with UniqueFactorizationDomain.Conv[Element[C, Array[N]]] {
  given instance: PolynomialOverFieldWithGB[C, N] = this
  def newInstance(pp: ArrayPowerProduct[N]) = new PolynomialOverFieldWithGB(using ring, pp)
}
