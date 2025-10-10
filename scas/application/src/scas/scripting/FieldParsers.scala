package scas.scripting

import Parsers.*
import scala.compiletime.deferred

trait FieldParsers[T] extends UFDParsers[T] {
  given structure: scas.structure.commutative.Field[T] = deferred
  override def unsignedFactor: Parser[T] = base ~ opt(("**" | "^") ~> Int.factor) ^^ {
    case x ~ option => option match {
      case Some(exp) => x \ exp
      case None => x
    }
  }
}
