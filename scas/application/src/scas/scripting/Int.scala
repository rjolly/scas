package scas.scripting

import Parsers._
import scala.annotation.{nowarn, tailrec}
import scala.collection.immutable.{Map, SortedMap}
import scas.base.BigInteger
import BigInteger.int2bigInt

object Int extends OrderedRingParsers[BigInteger] {
  given structure: BigInteger.Impl = BigInteger
  def number: Parser[BigInteger] = """\d+""".r ^^ { BigInteger(_) }
  def function1: Parser[BigInteger] = ("factorial") ~ ("(" ~> expr) <~ ")" ^^ {
    case "factorial" ~ x if (x > 0) => factorial(x)
  }
  @nowarn("msg=match may not be exhaustive")
  def function2: Parser[BigInteger] = ("div" | "mod") ~ ("(" ~> expr) ~ ("," ~> expr) <~ ")" ^^ {
    case "div" ~ x ~ y => x / y
    case "mod" ~ x ~ y => x % y
  }
  def function: Parser[BigInteger] = function1 | function2
  def base: Parser[BigInteger] = number | function | "(" ~> expr <~ ")"

  def factorial(x: BigInteger): BigInteger = factorial(BigInteger("1"), x)

  @tailrec final def factorial(res: BigInteger, x: BigInteger): BigInteger = if (x > 1) factorial(x * res, x - 1) else res

  def factor(x: BigInteger): Map[BigInteger, Int] = {
    assert(x > 0)
    factor(x, SortedMap.empty[BigInteger, Int], primes)
  }

  @tailrec final def factor(x: BigInteger, map: Map[BigInteger, Int], primes: LazyList[BigInteger]): Map[BigInteger, Int] = {
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
