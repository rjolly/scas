package rings

import cc.redberry.rings.io.Coder
import cc.redberry.rings.poly.MultivariateRing
import cc.redberry.rings.poly.multivar.MultivariatePolynomial
import cc.redberry.rings.poly.multivar.DegreeVector;
import java.util.Comparator

trait MultivariatePolynomialRing[C : Ring](monomialOrder: Comparator[DegreeVector], variables: String*) extends Ring[MultivariatePolynomial[C]] {
  val ring: MultivariateRing[MultivariatePolynomial[C]] = MultivariateRing(MultivariatePolynomial.zero(variables.size, Ring[C].ring, monomialOrder))
  override def coder = Coder.mkMultivariateCoder(ring, Ring[C].coder, variables: _*)
  def gens = (for (i <- 0 until ring.nVariables()) yield ring.variable(i)).toList
}

object MultivariatePolynomialRing {
  def apply[C : Ring](monomialOrder: Comparator[DegreeVector], variables: String*) = new conversion.MultivariatePolynomialRing(monomialOrder, variables: _*)
}

