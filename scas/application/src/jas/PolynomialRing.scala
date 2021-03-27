package jas

import edu.jas.poly.GenPolynomialRing
import edu.jas.poly.GenPolynomial
import edu.jas.structure.RingElem
import scala.collection.JavaConverters.asScalaBufferConverter

class PolynomialRing[C <: RingElem[C]](using val factory: GenPolynomialRing[C]) extends Ring[GenPolynomial[C]] {
  given PolynomialRing[C] = this
  def gens = factory.getGenerators().asScala.toArray

  given coef2poly[U](using c: U => C): (U => GenPolynomial[C]) = x => factory.valueOf(c(x))
}
