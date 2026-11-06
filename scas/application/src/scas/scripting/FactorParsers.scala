package scas.scripting

import Parsers.*
import scala.annotation.nowarn
import scas.factors.IntegerFactors

object FactorParsers extends MonoidParsers[IntegerFactors] {
  override given structure: IntegerFactors.type = IntegerFactors
  @nowarn("msg=match may not be exhaustive")
  def base: Parser[IntegerFactors] = ("factor") ~ ("(" ~> Int.expr) <~ ")" ^^ {
    case "factor" ~ x => structure(x)
  }
}
