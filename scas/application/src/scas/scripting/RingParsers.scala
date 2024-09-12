package scas.scripting

import Parsers._

trait RingParsers[T] extends MonoidParsers[T] {
  given structure: scas.structure.Ring[T]
  def base: Parser[T]
  def unsignedFactor: Parser[T] = base ~ opt(("**" | "^") ~> Int.unsignedFactor) ^^ {
    case x ~ option => option match {
      case Some(exp) => x \ exp
      case None => x
    }
  }
  override def factor: Parser[T] = opt("-") ~ unsignedFactor ^^ {
    case option ~ factor => option match {
      case Some(sign) => -factor
      case None => factor
    }
  }
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
  override def expr: Parser[T] = term ~ rep(("+" | "-") ~ unsignedTerm) ^^ {
    case term ~ list => list.foldLeft(term) {
      case (x, "+" ~ y) => x + y
      case (x, "-" ~ y) => x - y
    }
  }
}
