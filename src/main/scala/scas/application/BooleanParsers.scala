package scas.application

import Parsers._
import scas.Boolean

object BooleanParsers extends UFDParsers[Boolean] {
  val structure = Boolean
  def boolean: Parser[Boolean] = ("true" | "false") ^^ { _.toBoolean }
  override def comparison: Parser[Boolean] = base ~ ("=>" | "=" | "<>") ~ base ^^ {
    case x ~ "=>" ~ y => y || !x
    case x ~ "=" ~ y => x == y
    case x ~ "<>" ~ y => x != y
  }
  def negation: Parser[Boolean] = "!" ~> base ^^ { case x => !x }
  def function: Parser[Boolean] = RF.comparison | ComplexParsers.comparison | comparison | negation
  def base: Parser[Boolean] = boolean | "(" ~> expr <~ ")"
  override def term: Parser[Boolean] = function | base
  override def expr: Parser[Boolean] = term ~ ((("&" | "|" | "^") ~ term)*) ^^ {
    case term ~ list => (term /: list) {
      case (x, "&" ~ y) => x && y
      case (x, "|" ~ y) => x || y
      case (x, "^" ~ y) => x ^ y
    }
  }
}
