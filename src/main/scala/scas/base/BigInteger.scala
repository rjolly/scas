package scas.base

import scala.annotation.tailrec
import scala.collection.{Map, SortedMap}
import scas.structure.ordered.EuclidianDomain
import scas.{int2bigInteger, long2bigInteger, Variable}
import scas.Implicits.{infixOrderingOps, infixUFDOps}

trait BigIntegerLike extends EuclidianDomain[BigInteger] {
  implicit val self = this
  override def isZero(x: BigInteger) = signum(x) == 0
  def apply(s: String) = new BigInteger(s)
  def apply(l: Long) = l
  def random(numbits: Int)(implicit rnd: java.util.Random) = {
    val r = new BigInteger(numbits, rnd)
    if (rnd.nextBoolean()) r.negate() else r
  }
  def characteristic = 0
  def isUnit(x: BigInteger) = abs(x).isOne
  override def pow(x: BigInteger, exp: BigInteger) = x.pow(exp.intValue())
  override def negate(x: BigInteger) = x.negate()
  override def abs(x: BigInteger) = x.abs()
  override def signum(x: BigInteger) = x.signum()
  def norm(x: BigInteger) = abs(x).shiftLeft(1).add(if (signum(x) < 0) 1 else 0)
  def gcd(x: BigInteger, y: BigInteger) = x.gcd(y)
  override def divide(x: BigInteger, y: BigInteger) = x.divide(y)
  override def remainder(x: BigInteger, y: BigInteger) = x.remainder(y)
  def divideAndRemainder(x: BigInteger, y: BigInteger) = {
    val Array(q, r) = x.divideAndRemainder(y)
    (q, r)
  }
  def plus(x: BigInteger, y: BigInteger) = x.add(y)
  def minus(x: BigInteger, y: BigInteger) = x.subtract(y)
  def times(x: BigInteger, y: BigInteger) = x.multiply(y)
  def compare(x: BigInteger, y: BigInteger) = x.compareTo(y)
  override def toCode(x: BigInteger, precedence: Int) = {
    if (x.bitLength < 32) x.toString
    else if (x.bitLength < 64) x.toString + "l"
    else "BigInteger(\"" + x + "\")"
  }
  override def toString = "ZZ"
  def toMathML(x: BigInteger) = <cn>{x}</cn>
  def toMathML = <integers/>
  def function(x: BigInteger, a: Variable) = Function(x.doubleValue())

  def factorial(x: BigInteger): BigInteger = factorial(1, x)

  @tailrec final def factorial(res: BigInteger, x: BigInteger): BigInteger = if (x > 1) factorial(x * res, x - 1) else res

  def factor(x: BigInteger): Map[BigInteger, Int] = {
    assert(x > 0)
    factor(x, SortedMap.empty[BigInteger, Int](this), primes)
  }

  @tailrec final def factor(x: BigInteger, map: Map[BigInteger, Int], primes: Stream[BigInteger]): Map[BigInteger, Int] = {
    val y = primes.head
    if (x >< 1) map
    else if (y * y > x) map + ((x, map.getOrElse(x, 0) + 1))
    else if (y | x) factor(x / y, map + ((y, map.getOrElse(y, 0) + 1)), primes)
    else factor(x, map, primes.tail)
  }

  def sieve(n: BigInteger): Stream[BigInteger] = {
    if (primes.takeWhile(p => p * p <= n).exists(n % _ >< 0)) sieve(n + BigInteger(2))
    else Stream.cons(n, sieve(n + BigInteger(2)))
  }

  val primes: Stream[BigInteger] = Stream.cons(2, sieve(3))
}
