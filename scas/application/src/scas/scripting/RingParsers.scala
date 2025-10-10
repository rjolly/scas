package scas.scripting

import Parsers.*
import scala.annotation.nowarn
import scala.compiletime.deferred

trait RingParsers[T] extends StructureParsers[T] {
  given structure: scas.structure.Ring[T] = deferred
  def base: Parser[T]
  def unsignedFactor: Parser[T] = base ~ opt(("**" | "^") ~> Int.unsignedFactor) ^^ {
    case x ~ option => option match {
      case Some(exp) => x \ exp
      case None => x
    }
  }
  def factor: Parser[T] = opt("-") ~ unsignedFactor ^^ {
    case option ~ factor => option match {
      case Some(sign) => -factor
      case None => factor
    }
  }
  @nowarn("msg=match may not be exhaustive")
  def unsignedTerm: Parser[T] = unsignedFactor ~ rep("*" ~ factor) ^^ {
    case factor ~ list => list.foldLeft(factor) {
      case (x, "*" ~ y) => x * y
    }
  }
  def term: Parser[T] = opt("-") ~ unsignedTerm ^^ {
    case option ~ term => option match {
      case Some(sign) => -term
      case None => term
    }
  }
  @nowarn("msg=match may not be exhaustive")
  def expr: Parser[T] = term ~ rep(("+" | "-") ~ unsignedTerm) ^^ {
    case term ~ list => list.foldLeft(term) {
      case (x, "+" ~ y) => x + y
      case (x, "-" ~ y) => x - y
    }
  }
}
