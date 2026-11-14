package scas.scripting

import Parsers.*
import scala.annotation.nowarn
import scala.compiletime.deferred

trait UFDParsers[T] extends RingParsers[T] {
  given structure: scas.structure.commutative.UniqueFactorizationDomain[T] = deferred
  override def unsignedFactor: Parser[T] = base ~ opt(("**" | "^") ~> RationalParsers.unsignedFactor) ^^ {
    case x ~ option => option match {
      case Some(exp) => structure.pow(x, exp)
      case None => x
    }
  }
  @nowarn("msg=match may not be exhaustive")
  override def unsignedTerm: Parser[T] = unsignedFactor ~ rep(("*" | "/" | "%") ~ factor) ^^ {
    case factor ~ list => list.foldLeft(factor) {
      case (x, "*" ~ y) => x * y
      case (x, "/" ~ y) => x / y
      case (x, "%" ~ y) => x % y
    }
  }
}
