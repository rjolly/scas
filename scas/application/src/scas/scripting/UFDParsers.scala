package scas.scripting

import Parsers._

trait UFDParsers[T] extends RingParsers[T] {
  given structure: scas.structure.commutative.UniqueFactorizationDomain[T]
  override def unsignedTerm: Parser[T] = unsignedFactor ~ rep(("*" | "/" | "%") ~ factor) ^^ {
    case factor ~ list => list.foldLeft(factor) {
      case (x, "*" ~ y) => x.convert * y.convert
      case (x, "/" ~ y) => x.convert / y.convert
      case (x, "%" ~ y) => x.convert % y.convert
    }
  }
}
