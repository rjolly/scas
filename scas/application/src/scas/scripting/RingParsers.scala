package scas.scripting

import Parsers._

trait RingParsers[T] extends StructureParsers[T] {
  given structure: scas.structure.Ring[T]
  def base: Parser[T]
  def unsignedFactor: Parser[T] = base ~ opt(("**" | "^") ~> Int.unsignedFactor) ^^ {
    case x ~ option => option match {
      case Some(exp) => x.convert \ exp
      case None => x
    }
  }
  def factor: Parser[T] = opt("-") ~ unsignedFactor ^^ {
    case option ~ factor => option match {
      case Some(sign) => -factor.convert
      case None => factor
    }
  }
  def unsignedTerm: Parser[T] = unsignedFactor ~ rep("*" ~ factor) ^^ {
    case factor ~ list => list.foldLeft(factor) {
      case (x, "*" ~ y) => x.convert * y.convert
    }
  }
  def term: Parser[T] = opt("-") ~ unsignedTerm ^^ {
    case option ~ term => option match {
      case Some(sign) => -term.convert
      case None => term
    }
  }
  def expr: Parser[T] = term ~ rep(("+" | "-") ~ unsignedTerm) ^^ {
    case term ~ list => list.foldLeft(term) {
      case (x, "+" ~ y) => x.convert + y.convert
      case (x, "-" ~ y) => x.convert - y.convert
    }
  }
}
