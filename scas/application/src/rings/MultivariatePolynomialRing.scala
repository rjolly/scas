package rings

import cc.redberry.rings.io.Coder
import cc.redberry.rings.poly.MultivariateRing
import cc.redberry.rings.poly.multivar.MultivariatePolynomial
import cc.redberry.rings.poly.multivar.DegreeVector;
import scas.util.{Conversion, unary_~}
import java.util.Comparator

class MultivariatePolynomialRing[C : Ring](monomialOrder: Comparator[DegreeVector], variables: String*) extends Ring[MultivariatePolynomial[C]] with Ring.Ops[MultivariatePolynomial[C]] {
  given MultivariatePolynomialRing[C] = this
  val ring: MultivariateRing[MultivariatePolynomial[C]] = MultivariateRing(MultivariatePolynomial.zero(variables.size, Ring[C].ring, monomialOrder))
  override def coder = Coder.mkMultivariateCoder(ring, Ring[C].coder, variables: _*)
  def gens = (for (i <- 0 until ring.nVariables()) yield ring.variable(i)).toArray

  given coef2poly[D : Conversion[C]]: (D => MultivariatePolynomial[C]) = x => ring.factory().createConstant(~x)
}
