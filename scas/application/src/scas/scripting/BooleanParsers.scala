package scas.scripting

import Parsers._
import scala.annotation.nowarn
import scas.base.Boolean

object BooleanParsers extends BooleanRingParsers[Boolean] {
  given structure: Boolean.Impl = Boolean
  def boolean: Parser[Boolean] = ("true" | "false") ^^ { _.toBoolean }
  def base: Parser[Boolean] = boolean | "(" ~> expr <~ ")"
  @nowarn("msg=match may not be exhaustive")
  override def comparison: Parser[Boolean] = term ~ rep(("=>" | "=" | "<>") ~ term) ^^ {
    case term ~ list => list.foldLeft(term) {
      case (x, "=>" ~ y) => x >> y
      case (x, "=" ~ y) => x >< y
      case (x, "<>" ~ y) => x <> y
    }
  }
  override def impl: Parser[Boolean] = DoubleParsers.comparison | RationalParsers.comparison | ComplexParsers.comparison | RFParsers.comparison | BAParsers.comparison | comparison
}
