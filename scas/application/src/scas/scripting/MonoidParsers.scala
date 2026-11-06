package scas.scripting

import Parsers.*
import scala.annotation.nowarn
import scala.compiletime.deferred

trait MonoidParsers[T] extends StructureParsers[T] {
  given structure: scas.structure.Monoid[T] = deferred
  def base: Parser[T]
  def unsignedFactor: Parser[T] = base ~ opt(("**" | "^") ~> Int.unsignedFactor) ^^ {
    case x ~ option => option match {
      case Some(exp) => x \ exp
      case None => x
    }
  }
  @nowarn("msg=match may not be exhaustive")
  def expr: Parser[T] = unsignedFactor ~ rep("*" ~ unsignedFactor) ^^ {
    case factor ~ list => list.foldLeft(factor) {
      case (x, "*" ~ y) => x * y
    }
  }
}
