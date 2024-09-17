package scas.scripting

import Parsers._

object FactorParsers extends StructureParsers[FS] {
  given structure: FS.type = FS
  def expr: Parser[FS] = ("factor") ~ ("(" ~> Int.expr) <~ ")" ^^ {
    case "factor" ~ x => structure.factor(x)
  }
}
