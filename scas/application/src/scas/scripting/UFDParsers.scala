package scas.scripting

import Parsers.*
import scala.annotation.nowarn
import scala.compiletime.deferred

trait UFDParsers[T] extends RingParsers[T] {
  given structure: scas.structure.commutative.UniqueFactorizationDomain[T] = deferred
  @nowarn("msg=match may not be exhaustive")
  override def unsignedTerm: Parser[T] = unsignedFactor ~ rep(("*" | "/" | "%") ~ factor) ^^ {
    case factor ~ list => list.foldLeft(factor) {
      case (x, "*" ~ y) => x.convert * y.convert
      case (x, "/" ~ y) => x.convert / y.convert
      case (x, "%" ~ y) => x.convert % y.convert
    }
  }
}
