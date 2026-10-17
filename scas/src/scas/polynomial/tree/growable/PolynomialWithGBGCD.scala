package scas.polynomial.tree.growable

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.power.POT
import scas.power.growable.ArrayPowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class PolynomialWithGBGCD[C : UniqueFactorizationDomain, N : {ArrayPowerProduct, Numeric, ClassTag}] extends TreePolynomial[C, Array[N]] with scas.polynomial.growable.ufd.PolynomialWithGB[Element[C, Array[N]], C, N] with UniqueFactorizationDomain.Conv[Element[C, Array[N]]] {
  given instance: PolynomialWithGBGCD[C, N] = this
  def newInstance(pp: POT[N]) = new scas.polynomial.tree.PolynomialWithGBGCD(using ring, pp)
}
