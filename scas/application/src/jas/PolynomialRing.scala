package jas

import edu.jas.poly.GenPolynomialRing
import edu.jas.poly.GenPolynomial
import edu.jas.structure.RingElem
import scala.jdk.CollectionConverters.ListHasAsScala

class PolynomialRing[C <: RingElem[C] : GenPolynomialRing] extends Ring[GenPolynomial[C]] {
  val factory: GenPolynomialRing[C] = summon[GenPolynomialRing[C]]
  def gens = factory.getGenerators().asScala.toArray
}
