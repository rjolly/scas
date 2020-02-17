import scas.math.Ordering
import scas.structure.Ring
import edu.jas.arith.BigInteger
import edu.jas.poly.GenPolynomialRing
import edu.jas.poly.GenPolynomial
import edu.jas.structure.RingElem
import edu.jas.structure.RingFactory
import scala.collection.JavaConverters.asScalaBufferConverter

package object jas with
  class JasRing[T <: RingElem[T]](val factory: RingFactory[T]) extends Ring[T] with Ordering[T] with
    def (x: T) + (y: T) = x.sum(y)
    def (x: T) - (y: T) = x.subtract(y)
    def (x: T) * (y: T) = x.multiply(y)
    def compare(x: T, y: T) = x.compareTo(y)
    def (x: T) isZero = x.isZERO()
    def (x: T) isOne = x.isONE()
    def zero = factory.getZERO()
    def one = factory.getONE()

  given ZZ as JasRing[BigInteger] = JasRing(BigInteger())

  class PolyRing[C <: RingElem[C]](override val factory: GenPolynomialRing[C]) extends JasRing(factory) with
    def gens = factory.getGenerators().asScala.toArray

  given jas2scas[C <: RingElem[C]](using factory: GenPolynomialRing[C]) as PolyRing(factory)

  given int2bigInt as Conversion[Int, BigInteger] = BigInteger(_)
  given long2bigInt as Conversion[Long, BigInteger] = BigInteger(_)
  given coef2poly[C <: RingElem[C]](using r: PolyRing[C]) as Conversion[C, GenPolynomial[C]] = r.factory.valueOf(_)
  given recurse2poly[U, C <: RingElem[C]](using r: PolyRing[C], c: Conversion[U, C]) as Conversion[U, GenPolynomial[C]] = r.factory.valueOf(_)
