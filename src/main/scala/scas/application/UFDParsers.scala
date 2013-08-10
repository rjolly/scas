package scas.application

import Parsers._
import scas.structure.UniqueFactorizationDomain
import scas.Implicits.infixUFDOps
import scas.pow

trait UFDParsers[T] extends RingParsers[T] {
  implicit def structure: UniqueFactorizationDomain[T]
  override def unsignedFactor: Parser[T] = base ~ ((("**" | "^") ~> Int.factor)?) ^^ {
    case x ~ option => option match {
      case Some(exp) => pow(x, exp)
      case None => x
    }
  }
  override def unsignedTerm: Parser[T] = unsignedFactor ~ (("*" ~ factor | "/" ~ factor)*) ^^ {
    case factor ~ list => (factor /: list) {
      case (x, "*" ~ y) => x * y
      case (x, "/" ~ y) => x / y
    }
  }
}
