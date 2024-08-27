package scas.scripting

import java.lang.Integer
import scala.annotation.tailrec
import scas.util.{Conversion, unary_~}
import scala.collection.immutable.SortedMap
import scas.base.BigInteger
import BigInteger.given
import Factors.Element

type FS = Element[BigInteger, Int]

object FS extends Factors[BigInteger, Int] {
  def empty = SortedMap.empty[BigInteger, Int]

  def apply[U: Conversion[BigInteger]](x: U) = super.apply(~x)
  override def apply(x: BigInteger) = if(x.isZero) zero else factor(BigInteger.abs(x), SortedMap.empty[BigInteger, Integer], primes).mapInt*this(BigInteger.signum(x))

  @tailrec final def factor(x: BigInteger, map: Map[BigInteger, Integer], primes: LazyList[BigInteger]): Map[BigInteger, Integer] = {
    val y = primes.head
    if (x >< 1) map
    else if (y * y > x) map + ((x, map.getOrElse(x, Integer.valueOf(0)).intValue() + 1))
    else if (y | x) factor(x / y, map + ((y, map.getOrElse(y, Integer.valueOf(0)).intValue() + 1)), primes)
    else factor(x, map, primes.tail)
  }

  extension (map: Map[BigInteger, Integer]) def mapInt = map.foldLeft(one) { (l, r) =>
    val (a, b) = r
    l + ((a, b.intValue()))
  }

  def sieve(n: BigInteger): LazyList[BigInteger] = {
    if (primes.takeWile_square_lteq(n).exists_factorOf(n)) sieve(n + 2)
    else n #:: sieve(n + 2)
  }

  extension (s: LazyList[BigInteger]) def takeWile_square_lteq(n: BigInteger): LazyList[BigInteger] = {
    val p = s.head
    if (s.isEmpty || !(p * p <= n)) LazyList.empty
    else s.head #:: s.tail.takeWile_square_lteq(n)
  }

  extension (s: LazyList[BigInteger]) def exists_factorOf(n: BigInteger) = {
    var res = false
    val it = s.iterator
    while (!res && it.hasNext) res = it.next() | n
    res
  }

  val primes: LazyList[BigInteger] = BigInteger("2") #:: sieve(BigInteger("3"))
}
