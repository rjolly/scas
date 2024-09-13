package scas.scripting

import Parsers._
import scas.base.BigInteger
import BigInteger.given
import Factors.Element

type FS = Element[BigInteger, Int]

object FactorParsers extends RingParsers[FS] {
  given structure: Factors[BigInteger, Int] = new Factors
  import structure.ring2factors
  def base: Parser[FS] = ("factor") ~ ("(" ~> Int.expr) <~ ")" ^^ {
    case "factor" ~ x if (x <> 0) => factor(x)
  }
  def factor(x: BigInteger) = Int.factor(BigInteger.abs(x))*BigInteger.signum(x)
}
