package scas.application

import Parsers._
import scas.structure.ordered.Structure
import scas.Implicits.{infixOps, infixOrderingOps}

trait OrderingParsers[T] extends StructureParsers[T] {
  implicit def structure: Structure[T]
  override def comparison: Parser[Boolean] = expr ~ ("=" | "<>" | "<=" | "<" | ">=" | ">") ~ expr ^^ {
    case x ~ "=" ~ y => x >< y
    case x ~ "<>" ~ y => x <> y
    case x ~ "<=" ~ y => x <= y
    case x ~ "<" ~ y => x < y
    case x ~ ">=" ~ y => x >= y
    case x ~ ">" ~ y => x > y
  }
}
