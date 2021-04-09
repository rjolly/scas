package jas

import edu.jas.poly.GenPolynomialRing
import edu.jas.poly.GenPolynomial
import edu.jas.structure.RingElem
import scala.collection.JavaConverters.asScalaBufferConverter
import scas.util.Conversion

class PolynomialRing[C <: RingElem[C] : GenPolynomialRing] extends Ring[GenPolynomial[C]] {
  given PolynomialRing[C] = this
  val factory: GenPolynomialRing[C] = summon[GenPolynomialRing[C]]
  def gens = factory.getGenerators().asScala.toArray

  given coef2poly[D: Conversion[C]]: (D => GenPolynomial[C]) = jas.coef2poly
}
