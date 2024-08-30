package scas.scripting

import Parsers._

trait BooleanRingParsers[T] extends RingParsers[T] {
  given structure: scas.structure.BooleanRing[T]
  override def term: Parser[T] = opt("!") ~ base ^^ {
    case option ~ base => option match {
      case Some(sign) => !base
      case None => base
    }
  }
  def impl: Parser[T] = term ~ rep("=>" ~ term) ^^ {
    case term ~ list => list.foldLeft(term) {
      case (x, "=>" ~ y) => x >> y
    }
  }
  def conj: Parser[T] = impl ~ rep("&" ~ impl) ^^ {
    case impl ~ list => list.foldLeft(impl) {
      case (x, "&" ~ y) => x && y
    }
  }
  def disj: Parser[T] = conj ~ rep("^" ~ conj) ^^ {
    case conj ~ list => list.foldLeft(conj) {
      case (x, "^" ~ y) => x ^ y
    }
  }
  override def expr: Parser[T] = disj ~ rep("|" ~ disj) ^^ {
    case disj ~ list => list.foldLeft(disj) {
      case (x, "|" ~ y) => x || y
    }
  }
}
