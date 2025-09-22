package scas.scripting

import Parsers.*
import scala.annotation.nowarn
import scala.compiletime.deferred

trait OrderingParsers[T] extends StructureParsers[T] {
  given structure: scas.structure.ordered.Structure[T] = deferred
  @nowarn("msg=match may not be exhaustive")
  override def comparison: Parser[Boolean] = expr ~ ("=" | "<>" | "<=" | "<" | ">=" | ">") ~ expr ^^ {
    case x ~ "=" ~ y => x.convert >< y.convert
    case x ~ "<>" ~ y => x.convert <> y.convert
    case x ~ "<=" ~ y => x.convert <= y.convert
    case x ~ "<" ~ y => x.convert < y.convert
    case x ~ ">=" ~ y => x.convert >= y.convert
    case x ~ ">" ~ y => x.convert > y.convert
  }
}
