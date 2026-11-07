package scas.factors

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scas.base.BigInteger
import BigInteger.given
import Factors.Element

type IntegerFactors = Element[BigInteger, Int]

object IntegerFactors extends Factors[BigInteger, Int] {
  def empty = SortedMap.empty[BigInteger, Int]

  def apply(x: BigInteger, dummy: Boolean) = this(x, one, primes)

  @tailrec final def apply(x: BigInteger, map: IntegerFactors, primes: LazyList[BigInteger]): IntegerFactors = {
    val y = primes.head
    if y * y > x then map * fromRing(x)
    else if y | x then this(x / y, map * fromRing(y), primes)
    else this(x, map, primes.tail)
  }

  def sieve(n: BigInteger): LazyList[BigInteger] = {
    if primes.takeWhile(p => p * p <= n).exists(_ | n) then sieve(n + 2)
    else n #:: sieve(n + 2)
  }

  val primes: LazyList[BigInteger] = BigInteger("2") #:: sieve(BigInteger("3"))
}
