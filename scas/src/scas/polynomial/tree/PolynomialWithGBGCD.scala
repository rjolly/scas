package scas.polynomial.tree

import scas.math.Numeric
import scala.reflect.ClassTag
import scas.power.{ArrayPowerProduct, POT}
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class PolynomialWithGBGCD[C : UniqueFactorizationDomain, N : {ArrayPowerProduct, Numeric, ClassTag}] extends TreePolynomial[C, Array[N]] with scas.polynomial.ufd.PolynomialWithGB[Element[C, Array[N]], C, N] with UniqueFactorizationDomain.Conv[Element[C, Array[N]]] {
  given instance: PolynomialWithGBGCD[C, N] = this
  def newInstance(pp: POT[N]) = new PolynomialWithGBGCD(using ring, pp)
}
