import scas.math.Ordering
import scas.structure.Ring
import scala.util.FromDigits
import edu.jas.poly.GenPolynomialRing
import edu.jas.poly.GenPolynomial
import edu.jas.structure.RingElem
import edu.jas.structure.RingFactory
import scala.collection.JavaConverters.asScalaBufferConverter

package object jas with
  class JasRing[T <: RingElem[T] : RingFactory] extends Ring[T] with Ordering[T] with
    val factory = summon[RingFactory[T]]
    def (x: T) + (y: T) = x.sum(y)
    def (x: T) - (y: T) = x.subtract(y)
    def (x: T) * (y: T) = x.multiply(y)
    def compare(x: T, y: T) = x.compareTo(y)
    def (x: T) isZero = x.isZERO()
    def (x: T) isOne = x.isONE()
    def zero = factory.getZERO()
    def one = factory.getONE()

  type BigInteger = edu.jas.arith.BigInteger
  given ZZ as BigInteger = edu.jas.arith.BigInteger()
  given BigInteger as JasRing[BigInteger] with FromDigits[BigInteger] with
    def fromDigits(digits: String) = edu.jas.arith.BigInteger(digits)
  given jas2scas[C <: RingElem[C] : GenPolynomialRing] as JasRing[GenPolynomial[C]] with
    override val factory: GenPolynomialRing[C] = summon[GenPolynomialRing[C]]
    def gens = factory.getGenerators().asScala.toArray

  given int2bigInt as Conversion[Int, BigInteger] = BigInteger(_)
  given long2bigInt as Conversion[Long, BigInteger] = BigInteger(_)
  given coef2poly[C <: RingElem[C] : GenPolynomialRing] as Conversion[C, GenPolynomial[C]] = summon[GenPolynomialRing[C]].valueOf(_)
  given recurse2poly[U, C <: RingElem[C] : GenPolynomialRing](using Conversion[U, C]) as Conversion[U, GenPolynomial[C]] = summon[GenPolynomialRing[C]].valueOf(_)
