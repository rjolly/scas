package scas.application

import scas._
import Implicits.{CC, infixUFDOps}
import Parsers._

object ComplexParsers {
  import Complex.{sqrt, realPart, imaginaryPart, conjugate}

  def function: Parser[Complex] = ("sqrt" | "real" | "imag" | "conjugate") ~ ("(" ~> expr) <~ ")" ^^ {
    case "sqrt" ~ x if (x >< BigInteger(-1)) => sqrt(x)
    case "real" ~ x => realPart(x)
    case "imag" ~ x => imaginaryPart(x)
    case "conjugate" ~ x => conjugate(x)
  }
  def base: Parser[Complex] = RationalParsers.base ^^ { Complex(_) } | function | "(" ~> expr <~ ")"
  def unsignedFactor: Parser[Complex] = base ~ ((("**" | "^") ~> Int.factor)?) ^^ {
    case x ~ option => option match {
      case Some(exp) => pow(x, exp)
      case None => x
    }
  }
  def factor: Parser[Complex] = ("-"?) ~ unsignedFactor ^^ {
    case option ~ factor => option match {
      case Some(sign) => -factor
      case None => factor
    }
  }
  def unsignedTerm: Parser[Complex] = unsignedFactor ~ (("*" ~ factor | "/" ~ factor)*) ^^ {
    case factor ~ list => (factor /: list) {
      case (x, "*" ~ y) => x * y
      case (x, "/" ~ y) => x / y
    }
  }
  def term: Parser[Complex] = ("-"?) ~ unsignedTerm ^^ {
    case option ~ term => option match {
      case Some(sign) => -term
      case None => term
    }
  }
  def expr: Parser[Complex] = term ~ (("+" ~ unsignedTerm | "-" ~ unsignedTerm)*) ^^ {
    case term ~ list => (term /: list) {
      case (x, "+" ~ y) => x + y
      case (x, "-" ~ y) => x - y
    }
  }
  def comparison: Parser[Boolean] = expr ~ ("=" | "<>") ~ expr ^^ {
    case x ~ "=" ~ y => x >< y
    case x ~ "<>" ~ y => x <> y
  }
}
