package jas

import edu.jas.poly.GenPolynomialRing
import edu.jas.poly.GenPolynomial
import edu.jas.structure.RingElem
import scas.util.Conversion
import scala.jdk.CollectionConverters.ListHasAsScala
import PolynomialRing.Ops

class PolynomialRing[C <: RingElem[C] : GenPolynomialRing] extends Ring[GenPolynomial[C]] {
  given PolynomialRing[C] = this
  given Ops[C] = new Ops[C]
  val factory: GenPolynomialRing[C] = summon[GenPolynomialRing[C]]
  def gens = factory.getGenerators().asScala.toArray

  given coef2poly[D : Conversion[C]]: (D => GenPolynomial[C]) = jas.coef2poly
}

object PolynomialRing {
  class Ops[C <: RingElem[C] : PolynomialRing] extends Ring.Ops[GenPolynomial[C]]
}
