package scas.scripting

import Parsers._
import scala.annotation.nowarn
import scas.rendering.MathObject
import scas.base.Boolean

trait StructureParsers[T] {
  given structure: scas.structure.Structure[T]
  def expr: Parser[T]
  def obj: Parser[MathObject] = expr ^^ { MathObject(_) }
  @nowarn("msg=match may not be exhaustive")
  def comparison: Parser[Boolean] = expr ~ ("=" | "<>") ~ expr ^^ {
    case x ~ "=" ~ y => x.convert >< y.convert
    case x ~ "<>" ~ y => x.convert <> y.convert
  }
}
