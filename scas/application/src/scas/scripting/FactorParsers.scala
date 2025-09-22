package scas.scripting

import Parsers.*
import scala.annotation.nowarn

object FactorParsers extends RingParsers[FS] {
  given structure: FS.type = FS
  @nowarn("msg=match may not be exhaustive")
  def base: Parser[FS] = ("factor") ~ ("(" ~> Int.expr) <~ ")" ^^ {
    case "factor" ~ x => structure(x)
  }
}
