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

class Autogiven[T](self: T) {
  given T = self
}

given poly2scas[C <: RingElem[C] : GenPolynomialRing]: Ring[GenPolynomial[C]] with {
  extension (factory: GenPolynomialRing[C]) def gens = factory.getGenerators().asScala.toArray
}

given int2bigInt: (Int => BigInteger) = new BigInteger(_)
given long2bigInt: (Long => BigInteger) = new BigInteger(_)
given coef2poly[U, C <: RingElem[C] : GenPolynomialRing](using c: U => C): (U => GenPolynomial[C]) = x => summon[GenPolynomialRing[C]].valueOf(c(x))

given bigInt2scas[U](using c: U => BigInteger): (U => scas.BigInteger) = x => (c(x)).`val`

extension (a: Long) def \:(b: Long) = long2bigInt(a) \ bigInt2scas.apply(long2bigInt(b))
