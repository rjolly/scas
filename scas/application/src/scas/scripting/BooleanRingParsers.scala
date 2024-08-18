package scas.scripting

import Parsers._
import scala.annotation.nowarn

trait BooleanRingParsers[T] extends RingParsers[T] {
  given structure: scas.structure.BooleanRing[T]
  def negation: Parser[T] = "!" ~> base ^^ { case x => !x }
  @nowarn("msg=match may not be exhaustive")
  def function: Parser[T] = base ~ "=>" ~ base ^^ {
    case x ~ "=>" ~ y => x >> y
  }
  override def term: Parser[T] = function | negation | base
  def conj: Parser[T] = term ~ rep("&" ~ term) ^^ {
    case term ~ list => list.foldLeft(term) {
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
