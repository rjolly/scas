package jas

import edu.jas.poly.GenPolynomialRing
import edu.jas.poly.GenPolynomial
import edu.jas.structure.RingElem
import scala.collection.JavaConverters.asScalaBufferConverter
import scas.util.{Conversion, unary_~}

class PolynomialRing[C <: RingElem[C]](val factory: GenPolynomialRing[C]) extends Ring[GenPolynomial[C]] {
  given PolynomialRing[C] = this
  def gens = factory.getGenerators().asScala.toArray

  given coef2poly[D: Conversion[C]]: (D => GenPolynomial[C]) = x => factory.valueOf(~x)
}
