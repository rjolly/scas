package scas.application

import Parsers._
import scas.structure.Ring
import scas.Implicits.infixRingOps
import scas.pow

trait RingParsers[T] extends StructureParsers[T] {
  implicit def structure: Ring[T]
  def base: Parser[T]
  def unsignedFactor: Parser[T] = base ~ ((("**" | "^") ~> Int.unsignedFactor)?) ^^ {
    case x ~ option => option match {
      case Some(exp) => pow(x, exp)
      case None => x
    }
  }
  def factor: Parser[T] = ("-"?) ~ unsignedFactor ^^ {
    case option ~ factor => option match {
      case Some(sign) => -factor
      case None => factor
    }
  }
  def unsignedTerm: Parser[T] = unsignedFactor ~ (("*" ~ factor)*) ^^ {
    case factor ~ list => (factor /: list) {
      case (x, "*" ~ y) => x * y
    }
  }
  def term: Parser[T] = ("-"?) ~ unsignedTerm ^^ {
    case option ~ term => option match {
      case Some(sign) => -term
      case None => term
    }
  }
  def expr: Parser[T] = term ~ ((("+" | "-") ~ unsignedTerm)*) ^^ {
    case term ~ list => (term /: list) {
      case (x, "+" ~ y) => x + y
      case (x, "-" ~ y) => x - y
    }
  }
}
