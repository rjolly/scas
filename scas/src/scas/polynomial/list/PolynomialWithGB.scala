package scas.polynomial.list

import scas.math.Numeric
import scala.reflect.ClassTag
import scas.power.{ArrayPowerProduct, POT}
import scas.structure.commutative.UniqueFactorizationDomain
import scas.polynomial.ListPolynomial
import ListPolynomial.Element

class PolynomialWithGB[C : UniqueFactorizationDomain, N : {ArrayPowerProduct, Numeric, ClassTag}] extends ListPolynomial[C, Array[N]] with scas.polynomial.ufd.PolynomialWithGB[Element[C, Array[N]], C, N] with UniqueFactorizationDomain.Conv[Element[C, Array[N]]] {
  given instance: PolynomialWithGB[C, N] = this
  def newInstance(pp: POT[N]) = new PolynomialWithGB(using ring, pp)
}
