package scas.application

import Parsers._
import scas.structure.Structure
import scas.Implicits.infixOps

trait StructureParsers[T] {
  implicit def structure: Structure[T]
  def expr: Parser[T]
  def comparison: Parser[Boolean] = expr ~ ("=" | "<>") ~ expr ^^ {
    case x ~ "=" ~ y => x >< y
    case x ~ "<>" ~ y => x <> y
  }
}
