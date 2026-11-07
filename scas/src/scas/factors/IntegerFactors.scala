package scas.factors

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scas.base.BigInteger
import BigInteger.given
import Factors.Element

type IntegerFactors = Element[BigInteger, Int]

object IntegerFactors extends Factors[BigInteger, Int] {
  def empty = SortedMap.empty[BigInteger, Int]

  override def factor(x: BigInteger) = factor(x, one, primes)

  @tailrec final def factor(x: BigInteger, map: IntegerFactors, primes: LazyList[BigInteger]): IntegerFactors = {
    val y = primes.head
    if x >< 1 then map
    else if y * y > x then map + ((x, map.getOrElse(x, 0) + 1))
    else if y | x then factor(x / y, map + ((y, map.getOrElse(y, 0) + 1)), primes)
    else factor(x, map, primes.tail)
  }

  def sieve(n: BigInteger): LazyList[BigInteger] = {
    if primes.takeWhile(p => p * p <= n).exists(_ | n) then sieve(n + 2)
    else n #:: sieve(n + 2)
  }

  val primes: LazyList[BigInteger] = BigInteger("2") #:: sieve(BigInteger("3"))
}
