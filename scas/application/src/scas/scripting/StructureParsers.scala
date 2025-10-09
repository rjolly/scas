package scas.scripting

import Parsers.*
import scala.annotation.nowarn
import scala.compiletime.deferred
import scas.rendering.MathObject

trait StructureParsers[T] {
  given structure: () => scas.structure.Structure[T] = deferred
  def expr: Parser[T]
  def obj: Parser[MathObject] = expr ^^ { MathObject(_) }
  @nowarn("msg=match may not be exhaustive")
  def comparison: Parser[Boolean] = expr ~ ("=" | "<>") ~ expr ^^ {
    case x ~ "=" ~ y => x >< y
    case x ~ "<>" ~ y => x <> y
  }
}
