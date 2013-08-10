package scas.application

import scas._
import Implicits.QQ
import Parsers._

object RationalParsers extends OrderedUFDParsers[Rational] {
  val structure = QQ
  def base: Parser[Rational] = Int.base ^^ { Rational(_) } | "(" ~> expr <~ ")"
}
