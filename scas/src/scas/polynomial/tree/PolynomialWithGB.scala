package scas.polynomial.tree

import scas.math.Numeric
import scala.reflect.ClassTag
import scas.power.{ArrayPowerProduct, POT}
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.TreePolynomial
import TreePolynomial.Element

class PolynomialWithGB[C : UniqueFactorizationDomain, N : {ArrayPowerProduct, Numeric, ClassTag}] extends TreePolynomial[C, Array[N]] with scas.polynomial.ufd.PolynomialWithGB[Element[C, Array[N]], C, N] with UniqueFactorizationDomain.Conv[Element[C, Array[N]]] {
  given instance: PolynomialWithGB[C, N] = this
  def newInstance(pp: POT[N]) = new PolynomialWithGB(using ring, pp)
}
