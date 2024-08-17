package scas.scripting

import Parsers._
import scala.annotation.nowarn
import scas.base.Boolean

class BooleanParsers(val rf: RFParsers) extends UFDParsers[Boolean] {
  given structure: Boolean.Impl = Boolean
  def boolean: Parser[Boolean] = ("true" | "false") ^^ { _.toBoolean }
  @nowarn("msg=match may not be exhaustive")
  override def comparison: Parser[Boolean] = base ~ ("=>" | "=" | "<>") ~ base ^^ {
    case x ~ "=>" ~ y => y || !x
    case x ~ "=" ~ y => x == y
    case x ~ "<>" ~ y => x != y
  }
  def negation: Parser[Boolean] = "!" ~> base ^^ { case x => !x }
  def function: Parser[Boolean] = RationalParsers.comparison | rf.comparison | DoubleParsers.comparison | ComplexParsers.comparison | comparison | negation
  def base: Parser[Boolean] = boolean | "(" ~> expr <~ ")"
  override def term: Parser[Boolean] = function | base
  @nowarn("msg=match may not be exhaustive")
  override def expr: Parser[Boolean] = term ~ rep(("&&" | "||" | "^") ~ term) ^^ {
    case term ~ list => list.foldLeft(term) {
      case (x, "&&" ~ y) => x && y
      case (x, "||" ~ y) => x || y
      case (x, "^" ~ y) => x ^ y
    }
  }
}
