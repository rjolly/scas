import scas.math.Ordering
import scas.structure.Ring
import edu.jas.arith.BigInteger
import edu.jas.poly.GenPolynomialRing
import edu.jas.poly.GenPolynomial
import edu.jas.structure.RingElem
import edu.jas.structure.RingFactory

package object jas with
  given jas2scas[T <: RingElem[T] : RingFactory] as Ring[T] with Ordering[T] with
    def (x: T) + (y: T) = x.sum(y)
    def (x: T) - (y: T) = x.subtract(y)
    def (x: T) * (y: T) = x.multiply(y)
    def compare(x: T, y: T) = x.compareTo(y)
    def (x: T) isZero = x.isZERO()
    def (x: T) isOne = x.isONE()
    def zero = summon[RingFactory[T]].getZERO()
    def one = summon[RingFactory[T]].getONE()

  given ZZ as BigInteger = BigInteger()

  given Conversion[Int, BigInteger] = BigInteger(_)
  given Conversion[Long, BigInteger] = BigInteger(_)
  given coef2poly[C <: RingElem[C]](using r: GenPolynomialRing[C]) as Conversion[C, GenPolynomial[C]] = r.valueOf(_)
  given recurse2poly[U, C <: RingElem[C]](using r: GenPolynomialRing[C], c: Conversion[U, C]) as Conversion[U, GenPolynomial[C]] = r.valueOf(_)
