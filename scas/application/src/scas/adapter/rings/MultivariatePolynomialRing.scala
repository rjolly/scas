package scas.adapter.rings

import cc.redberry.rings.io.Coder
import cc.redberry.rings.poly.MultivariateRing
import cc.redberry.rings.poly.multivar.MultivariatePolynomial
import cc.redberry.rings.poly.multivar.DegreeVector;
import scas.util.{Conversion, unary_~}
import java.util.Comparator

trait MultivariatePolynomialRing[C : Ring](monomialOrder: Comparator[DegreeVector], variables: String*) extends Ring[MultivariatePolynomial[C]] {
  val ring: MultivariateRing[MultivariatePolynomial[C]] = MultivariateRing(MultivariatePolynomial.zero(variables.size, Ring[C].ring, monomialOrder))
  override def coder = Coder.mkMultivariateCoder(ring, Ring[C].coder, variables*)
  def gens = (for i <- 0 until ring.nVariables() yield ring.variable(i)).toList

  given coef2poly[D : Conversion[C]]: (D => MultivariatePolynomial[C]) = x => ring.factory().createConstant(~x)
}

object MultivariatePolynomialRing {
  def apply[C : Ring](monomialOrder: Comparator[DegreeVector], variables: String*) = new Conv(monomialOrder, variables*)

  class Conv[C : Ring](monomialOrder: Comparator[DegreeVector], variables: String*) extends MultivariatePolynomialRing[C](monomialOrder, variables*) with Ring.Conv[MultivariatePolynomial[C]] {
    given instance: Conv[C] = this
  }
}
