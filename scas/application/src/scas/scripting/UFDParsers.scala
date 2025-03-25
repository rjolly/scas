package scas.scripting

import Parsers._
import scala.annotation.nowarn

trait UFDParsers[T] extends RingParsers[T] {
  given structure: scas.structure.commutative.UniqueFactorizationDomain[T]
  @nowarn("msg=match may not be exhaustive")
  override def unsignedTerm: Parser[T] = unsignedFactor ~ rep(("*" | "/" | "%") ~ factor) ^^ {
    case factor ~ list => list.foldLeft(factor) {
      case (x, "*" ~ y) => x.convert * y.convert
      case (x, "/" ~ y) => x.convert / y.convert
      case (x, "%" ~ y) => x.convert % y.convert
    }
  }
}
