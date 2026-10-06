package scas.polynomial.tree.mutable

import scas.math.Numeric
import scala.reflect.ClassTag
import scas.power.{ArrayPowerProduct, POT}
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.mutable.TreePolynomial
import scas.polynomial.TreePolynomial.Element

class PolynomialWithGB[C : UniqueFactorizationDomain, N : {ArrayPowerProduct, Numeric, ClassTag}] extends TreePolynomial[C, Array[N]] with scas.polynomial.ufd.PolynomialWithGB[Element[C, Array[N]], C, N] with scas.polynomial.mutable.PolynomialWithGB[Element[C, Array[N]], C, Array[N]] with UniqueFactorizationDomain.Conv[Element[C, Array[N]]] {
  given instance: PolynomialWithGB[C, N] = this
  def newInstance(pp: POT[N]) = new PolynomialWithGB(using ring, pp)
}
