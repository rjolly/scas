package scas.scripting

import Parsers.*
import scala.annotation.nowarn
import scala.compiletime.deferred

trait BooleanRingParsers[T] extends RingParsers[T] {
  given structure: () => scas.structure.BooleanRing[T] = deferred
  override def term: Parser[T] = opt("!") ~ base ^^ {
    case option ~ base => option match {
      case Some(sign) => !base.convert
      case None => base
    }
  }
  @nowarn("msg=match may not be exhaustive")
  def impl: Parser[T] = term ~ rep("=>" ~ term) ^^ {
    case term ~ list => list.foldLeft(term) {
      case (x, "=>" ~ y) => x.convert >> y.convert
    }
  }
  @nowarn("msg=match may not be exhaustive")
  def conj: Parser[T] = impl ~ rep("&" ~ impl) ^^ {
    case impl ~ list => list.foldLeft(impl) {
      case (x, "&" ~ y) => x.convert && y.convert
    }
  }
  @nowarn("msg=match may not be exhaustive")
  def disj: Parser[T] = conj ~ rep("^" ~ conj) ^^ {
    case conj ~ list => list.foldLeft(conj) {
      case (x, "^" ~ y) => x.convert ^ y.convert
    }
  }
  @nowarn("msg=match may not be exhaustive")
  override def expr: Parser[T] = disj ~ rep("|" ~ disj) ^^ {
    case disj ~ list => list.foldLeft(disj) {
      case (x, "|" ~ y) => x.convert || y.convert
    }
  }
}
