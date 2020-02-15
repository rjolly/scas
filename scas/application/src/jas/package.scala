import scas.math.Equiv
import scas.math.Ordering
import scas.structure.Ring
import edu.jas.arith.BigInteger
import edu.jas.poly.GenPolynomialRing
import edu.jas.poly.GenPolynomial
import edu.jas.poly.TermOrder
import edu.jas.structure.RingElem
import edu.jas.structure.RingFactory
import collection.JavaConverters.asScalaBufferConverter

package object jas with
  given BigInteger as Ring[BigInteger] with Ordering[BigInteger] with
    def (x: BigInteger) + (y: BigInteger) = x.sum(y)
    def (x: BigInteger) - (y: BigInteger) = x.subtract(y)
    def (x: BigInteger) * (y: BigInteger) = x.multiply(y)
    def compare(x: BigInteger, y: BigInteger) = x.compareTo(y)
    def (x: BigInteger) isZero = x >< 0
    def (x: BigInteger) isOne = x >< 1
    def zero = 0
    def one = 1

  val ZZ = BigInteger()

  class PolyRing[C <: RingElem[C]](cf: RingFactory[C], v: Array[String], t: TermOrder) extends GenPolynomialRing(cf, v, t) with Ring[GenPolynomial[C]] with Equiv[GenPolynomial[C]]
    def gens = getGenerators().asScala.toArray
    def (x: GenPolynomial[C]) + (y: GenPolynomial[C]) = x.sum(y)
    def (x: GenPolynomial[C]) - (y: GenPolynomial[C]) = x.subtract(y)
    def (x: GenPolynomial[C]) * (y: GenPolynomial[C]) = x.multiply(y)
    def equiv(x: GenPolynomial[C], y: GenPolynomial[C]) = x.equals(y)
    def (x: GenPolynomial[C]) isZero = x.isZERO()
    def (x: GenPolynomial[C]) isOne = x.isONE()
    def zero = getZERO()
    def one = getONE()

  given Conversion[Int, BigInteger] = BigInteger(_)
  given Conversion[Long, BigInteger] = BigInteger(_)
  given coef2poly[C <: RingElem[C]](using r: PolyRing[C]) as Conversion[C, GenPolynomial[C]] = r.valueOf(_)
  given recurse2poly[U, C <: RingElem[C]](using r: PolyRing[C], c: Conversion[U, C]) as Conversion[U, GenPolynomial[C]] = r.valueOf(_)
