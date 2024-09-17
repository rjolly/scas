package scas.scripting

import Parsers._

object FactorParsers extends RingParsers[FS] {
  given structure: FS.type = FS
  def base: Parser[FS] = ("factor") ~ ("(" ~> Int.expr) <~ ")" ^^ {
    case "factor" ~ x => structure(x)
  }
}
