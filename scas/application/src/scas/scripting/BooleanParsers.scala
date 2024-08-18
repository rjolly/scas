package scas.scripting

import Parsers._
import scas.base.Boolean

object BooleanParsers extends BooleanRingParsers[Boolean] {
  given structure: Boolean.Impl = Boolean
  def boolean: Parser[Boolean] = ("true" | "false") ^^ { _.toBoolean }
  def base: Parser[Boolean] = boolean | "(" ~> expr <~ ")"
}
