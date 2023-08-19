package jas

import edu.jas.poly.GenPolynomialRing
import edu.jas.poly.GenPolynomial
import edu.jas.structure.RingElem
import scas.util.Conversion
import scala.jdk.CollectionConverters.ListHasAsScala
import PolynomialRing.Ops

class PolynomialRing[C <: RingElem[C] : GenPolynomialRing] extends Ring[GenPolynomial[C]] with Ops[C] {
  given PolynomialRing[C] = this
  val factory: GenPolynomialRing[C] = summon[GenPolynomialRing[C]]
  def gens = factory.getGenerators().asScala.toArray

  given coef2poly[D : Conversion[C]]: (D => GenPolynomial[C]) = jas.coef2poly
}

object PolynomialRing {
  trait Ops[C <: RingElem[C]] extends Ring.Ops[GenPolynomial[C]] { this: PolynomialRing[C] =>
  }
}
