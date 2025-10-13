package scas.adapter.jas

import edu.jas.poly.GenPolynomialRing
import edu.jas.poly.GenPolynomial
import edu.jas.structure.RingElem
import scas.util.Conversion
import scala.jdk.CollectionConverters.ListHasAsScala

class PolynomialRing[C <: RingElem[C] : GenPolynomialRing] extends Ring[GenPolynomial[C]] {
  val factory: GenPolynomialRing[C] = summon
  def gens = factory.getGenerators().asScala.toList

  given coef2poly: [D : Conversion[C]] => (D => GenPolynomial[C]) = scas.adapter.jas.coef2poly
}

object PolynomialRing {
  class Conv[C <: RingElem[C] : GenPolynomialRing] extends PolynomialRing[C] with Ring.Conv[GenPolynomial[C]] {
    given instance: Conv[C] = this
  }
}
