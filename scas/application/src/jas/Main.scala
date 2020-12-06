package jas

import scala.util.FromDigits
import edu.jas.poly.GenPolynomialRing
import edu.jas.poly.GenPolynomial
import edu.jas.structure.RingElem
import scala.collection.JavaConverters.asScalaBufferConverter

type BigInteger = edu.jas.arith.BigInteger

val ZZ = new BigInteger()

object BigInteger extends Ring[BigInteger](using ZZ) with FromDigits[BigInteger] {
  given this.type = this
  def fromDigits(digits: String) = new BigInteger(digits)
}
import BigInteger.given

given poly2scas[C <: RingElem[C] : GenPolynomialRing]: Ring[GenPolynomial[C]] with {
  extension (factory: GenPolynomialRing[C]) def gens = factory.getGenerators().asScala.toArray
}

given int2bigInt: Conversion[Int, BigInteger] = new BigInteger(_)
given long2bigInt: Conversion[Long, BigInteger] = new BigInteger(_)
given coef2poly[U, C <: RingElem[C] : GenPolynomialRing](using Conversion[U, C]): Conversion[U, GenPolynomial[C]] = (x: U) => (x: C): GenPolynomial[C]
given coef2poly[C <: RingElem[C] : GenPolynomialRing]: Conversion[C, GenPolynomial[C]] = summon[GenPolynomialRing[C]].valueOf(_)

given bigInt2scas[U](using Conversion[U, BigInteger]): Conversion[U, scas.BigInteger] = _.`val`

extension (a: Long) def \:(b: Long) = BigInteger(a) \ b
