package scas.scripting

import Parsers._
import scala.annotation.nowarn

trait OrderedStructure[T] extends scas.structure.ordered.Structure[T] with Structure[T] {
  @nowarn("msg=match may not be exhaustive")
  override def comparison: Parser[Boolean] = expr ~ ("=" | "<>" | "<=" | "<" | ">=" | ">") ~ expr ^^ {
    case x ~ "=" ~ y => x >< y
    case x ~ "<>" ~ y => x <> y
    case x ~ "<=" ~ y => x <= y
    case x ~ "<" ~ y => x < y
    case x ~ ">=" ~ y => x >= y
    case x ~ ">" ~ y => x > y
  }
}
