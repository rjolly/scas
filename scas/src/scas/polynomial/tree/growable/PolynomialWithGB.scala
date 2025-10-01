package scas.polynomial.tree.growable

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.power.POT
import scas.power.growable.ArrayPowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class PolynomialWithGB[C, N](using UniqueFactorizationDomain[C], ArrayPowerProduct[N])(using ClassTag[N], Numeric[N]) extends TreePolynomial[C, Array[N]] with scas.polynomial.ufd.growable.PolynomialWithGB[Element[C, Array[N]], C, N] with UniqueFactorizationDomain.Conv[Element[C, Array[N]]] {
  given instance: PolynomialWithGB[C, N] = this
  def newInstance(pp: POT[N]) = new scas.polynomial.tree.PolynomialWithGB(using ring, pp)
}
