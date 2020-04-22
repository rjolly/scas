package rings

import cc.redberry.rings.io.Coder
import cc.redberry.rings.poly.MultivariateRing
import cc.redberry.rings.poly.multivar.MultivariatePolynomial
import cc.redberry.rings.poly.multivar.DegreeVector;
import java.util.Comparator

class MultivariatePolynomialRing[C : RdbRing](monomialOrder: Comparator[DegreeVector], variables: String*) extends RdbRing[MultivariatePolynomial[C]] {
  val ring: MultivariateRing[MultivariatePolynomial[C]] = MultivariateRing(MultivariatePolynomial.zero(variables.size, RdbRing[C].ring, monomialOrder))
  override def coder = Coder.mkMultivariateCoder(ring, RdbRing[C].coder, variables: _*)
  def gens = (for (i <- 0 until ring.nVariables()) yield ring.variable(i)).toArray
}

object MultivariatePolynomialRing {
  def apply[C : MultivariatePolynomialRing] = summon[MultivariatePolynomialRing[C]]
}
