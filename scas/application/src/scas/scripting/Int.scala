package scas.scripting

import Parsers._
import scala.annotation.{nowarn, tailrec}
import scas.base.BigInteger
import BigInteger.int2bigInt

object Int extends OrderedRingParsers[BigInteger] {
  given structure: BigInteger.Impl = BigInteger
  def number: Parser[BigInteger] = """\d+""".r ^^ { BigInteger(_) }
  @nowarn("msg=match may not be exhaustive")
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

  def factorial(x: BigInteger): BigInteger = factorial(BigInteger.one, x)

  @tailrec final def factorial(res: BigInteger, x: BigInteger): BigInteger = if x > 1 then factorial(x * res, x - 1) else res
}
