package scas.scripting

import Parsers.*
import scas.base.Rational

object RationalParsers extends OrderedFieldParsers[Rational] {
  override given structure: Rational.Impl = Rational
  def base: Parser[Rational] = Int.base ^^ { Rational(_) } | "(" ~> expr <~ ")"
}
