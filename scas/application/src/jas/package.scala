import scas.math.Ordering
import scas.structure.{Monoid, Ring}
import scala.util.FromDigits
import edu.jas.poly.GenPolynomialRing
import edu.jas.poly.GenPolynomial
import edu.jas.structure.RingElem
import edu.jas.structure.RingFactory
import scala.collection.JavaConverters.asScalaBufferConverter

package object jas with
  class JasRing[T <: RingElem[T] : RingFactory] extends Ring[T] with Ordering[T] with
    def (x: T) + (y: T) = x.sum(y)
    def (x: T) - (y: T) = x.subtract(y)
    def (x: T) * (y: T) = x.multiply(y)
    def compare(x: T, y: T) = x.compareTo(y)
    def (x: T) isZero = x.isZERO()
    def (x: T) isOne = x.isONE()
    def zero = summon[RingFactory[T]].getZERO()
    def one = summon[RingFactory[T]].getONE()

  type BigInteger = edu.jas.arith.BigInteger

  given ZZ as BigInteger = new BigInteger()

  given BigInteger as JasRing[BigInteger] with FromDigits[BigInteger] with
    def fromDigits(digits: String) = new BigInteger(digits)
    def apply(x: BigInteger) = x

  given jas2scas[C <: RingElem[C] : GenPolynomialRing] as JasRing[GenPolynomial[C]] with
    def (factory: GenPolynomialRing[C]) gens = factory.getGenerators().asScala.toArray

  given id[T] as Conversion[T, T] = identity
  given int2bigInt as Conversion[Int, BigInteger] = new BigInteger(_)
  given long2bigInt as Conversion[Long, BigInteger] = new BigInteger(_)
  given coef2poly[U, C <: RingElem[C] : GenPolynomialRing](using Conversion[U, C]) as Conversion[U, GenPolynomial[C]] = summon[GenPolynomialRing[C]].valueOf(_)

  def [T: Monoid](a: T) \: (n: Long): T = a \ n
  def (a: Long) \: (n: Long) = BigInteger(a) \ n
