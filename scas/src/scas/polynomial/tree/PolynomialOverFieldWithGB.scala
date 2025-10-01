package scas.polynomial.tree

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.power.{POT, ArrayPowerProduct}
import scas.structure.commutative.{Field, UniqueFactorizationDomain}
import scas.polynomial.TreePolynomial
import scas.variable.Variable
import TreePolynomial.Element

class PolynomialOverFieldWithGB[C, N](using Field[C], ArrayPowerProduct[N])(using ClassTag[N], Numeric[N]) extends TreePolynomial[C, Array[N]] with scas.polynomial.ufd.PolynomialOverFieldWithGB[Element[C, Array[N]], C, N] with UniqueFactorizationDomain.Conv[Element[C, Array[N]]] {
  given instance: PolynomialOverFieldWithGB[C, N] = this
  def newInstance(pp: POT[N]) = new PolynomialOverFieldWithGB(using ring, pp)
}
