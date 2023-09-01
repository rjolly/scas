package jas

import edu.jas.poly.GenPolynomialRing
import edu.jas.poly.GenPolynomial
import edu.jas.structure.RingElem
import scas.util.Conversion
import scala.jdk.CollectionConverters.ListHasAsScala

class PolynomialRing[C <: RingElem[C] : GenPolynomialRing] extends Ring[GenPolynomial[C]] {
  given instance: PolynomialRing[C] = this
  val factory: GenPolynomialRing[C] = summon[GenPolynomialRing[C]]
  def gens = factory.getGenerators().asScala.toArray

  given coef2poly[D : Conversion[C]]: (D => GenPolynomial[C]) = jas.coef2poly
}
