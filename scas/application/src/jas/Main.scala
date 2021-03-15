package jas

import scala.util.FromDigits
import edu.jas.poly.GenPolynomialRing
import edu.jas.poly.GenPolynomial
import edu.jas.structure.RingElem

type BigInteger = edu.jas.arith.BigInteger

val ZZ = new BigInteger()

object BigInteger extends Ring[BigInteger](using ZZ) with FromDigits[BigInteger] {
  given BigInteger.type = this
  def fromDigits(digits: String) = new BigInteger(digits)
}
import BigInteger.given

extension [C <: RingElem[C]](factory: GenPolynomialRing[C]) def toScas = new PolynomialRing(using factory)

given int2bigInt: (Int => BigInteger) = new BigInteger(_)
given long2bigInt: (Long => BigInteger) = new BigInteger(_)
given coef2poly[U, C <: RingElem[C] : GenPolynomialRing](using c: U => C): (U => GenPolynomial[C]) = x => summon[GenPolynomialRing[C]].valueOf(c(x))

given bigInt2scas[U](using c: U => BigInteger): (U => scas.BigInteger) = x => (c(x)).`val`

extension (a: Long) def \:(b: Long) = long2bigInt(a) \ bigInt2scas.apply(long2bigInt(b))
