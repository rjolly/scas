package scas.application

import Parsers._
import scas.MathObject
import scas.structure.Structure
import scas.Implicits.infixOps

trait StructureParsers[T] {
  implicit def structure: Structure[T]
  def expr: Parser[T]
  def obj: Parser[MathObject] = expr ^^ { structure.render(_) }
  def comparison: Parser[Boolean] = expr ~ ("=" | "<>") ~ expr ^^ {
    case x ~ "=" ~ y => x >< y
    case x ~ "<>" ~ y => x <> y
  }
}
