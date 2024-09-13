package scas.scripting

import Parsers._

trait FieldParsers[T] extends UFDParsers[T] {
  given structure: scas.structure.commutative.Field[T]
  override def unsignedFactor: Parser[T] = base ~ opt(("**" | "^") ~> Int.factor) ^^ {
    case x ~ option => option match {
      case Some(exp) => x.convert \ exp
      case None => x
    }
  }
}
