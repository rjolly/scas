package scas.application

import Parsers._

object Boolean {
  def comparison: Parser[Boolean] = base ~ ("=>" | "=" | "<>") ~ base ^^ {
    case x ~ "=>" ~ y => y || !x
    case x ~ "=" ~ y => x == y
    case x ~ "<>" ~ y => x != y
  }
  def negation: Parser[Boolean] = "!" ~> base ^^ { case x => !x }
  def function: Parser[Boolean] = Int.comparison | ComplexParsers.comparison | RF.comparison | comparison | negation
  def base: Parser[Boolean] = boolean | "(" ~> expr <~ ")"
  def term: Parser[Boolean] = function | base
  def expr: Parser[Boolean] = term ~ (("&" ~ term | "|" ~ term | "^" ~ term)*) ^^ {
    case term ~ list => (term /: list) {
      case (x, "&" ~ y) => x && y
      case (x, "|" ~ y) => x || y
      case (x, "^" ~ y) => x ^ y
    }
  }
}
