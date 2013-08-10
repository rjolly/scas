package scas.application

import scas._
import Implicits.{ZZ, infixUFDOps, infixOrderingOps}
import BigInteger.factorial
import Parsers._

object Int extends OrderedRingParsers[BigInteger] {
  val structure = ZZ
  def number: Parser[BigInteger] = """\d+""".r ^^ { BigInteger(_) }
  def function1: Parser[BigInteger] = ("factorial") ~ ("(" ~> expr) <~ ")" ^^ {
    case "factorial" ~ x if (x > BigInteger(0)) => factorial(x)
  }
  def function2: Parser[BigInteger] = ("div" | "mod") ~ ("(" ~> expr) ~ ("," ~> expr) <~ ")" ^^ {
    case "div" ~ x ~ y => x / y
    case "mod" ~ x ~ y => x % y
  }
  def function: Parser[BigInteger] = function1 | function2
  def base: Parser[BigInteger] = number | function | "(" ~> expr <~ ")"
}
