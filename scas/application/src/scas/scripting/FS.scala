package scas.scripting

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scas.base.BigInteger
import BigInteger.given
import Factors.Element

type FS = Element[BigInteger, Int]

object FS extends Factors[BigInteger, Int] {
  def empty = SortedMap.empty[BigInteger, Int]

  def factor(x: BigInteger): FS = if(x.isZero) zero else factor(BigInteger.abs(x), one, primes)*BigInteger.signum(x)

  @tailrec final def factor(x: BigInteger, map: FS, primes: LazyList[BigInteger]): FS = {
    val y = primes.head
    if (x >< 1) map
    else if (y * y > x) map + ((x, map.getOrElse(x, 0) + 1))
    else if (y | x) factor(x / y, map + ((y, map.getOrElse(y, 0) + 1)), primes)
    else factor(x, map, primes.tail)
  }

  def sieve(n: BigInteger): LazyList[BigInteger] = {
    if (primes.takeWhile(p => p * p <= n).exists(_ | n)) sieve(n + 2)
    else n #:: sieve(n + 2)
  }

  val primes: LazyList[BigInteger] = BigInteger("2") #:: sieve(BigInteger("3"))
}
