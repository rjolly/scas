package scas.scripting

import Parsers._

trait UFDParsers[T] extends RingParsers[T] {
  given structure: scas.structure.commutative.UniqueFactorizationDomain[T]
  override def unsignedFactor: Parser[T] = base ~ opt(("**" | "^") ~> Int.factor) ^^ {
    case x ~ option => option match {
      case Some(exp) => x \ exp
      case None => x
    }
  }
  override def unsignedTerm: Parser[T] = unsignedFactor ~ rep(("*" | "/") ~ factor) ^^ {
    case factor ~ list => list.foldLeft(factor) {
      case (x, "*" ~ y) => x * y
      case (x, "/" ~ y) => x / y
    }
  }
}
