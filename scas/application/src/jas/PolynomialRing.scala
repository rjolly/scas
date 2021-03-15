package jas

import edu.jas.poly.GenPolynomialRing
import edu.jas.poly.GenPolynomial
import edu.jas.structure.RingElem
import scala.collection.JavaConverters.asScalaBufferConverter

class PolynomialRing[C <: RingElem[C]](using override val factory: GenPolynomialRing[C]) extends Ring[GenPolynomial[C]] {
  given PolynomialRing[C] = this
  def gens = factory.getGenerators().asScala.toArray
}
