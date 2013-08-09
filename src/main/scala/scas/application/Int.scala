package scas.application

import scas._
import Implicits.{ZZ, infixUFDOps, infixOrderingOps}
import Parsers._

object Int {
  def number: Parser[BigInteger] = """\d+""".r ^^ { BigInteger(_) }
  def function1: Parser[BigInteger] = ("factorial") ~ ("(" ~> expr) <~ ")" ^^ {
    case "factorial" ~ x if (x > BigInteger(0)) => BigInteger.factorial(x)
  }
  def function2: Parser[BigInteger] = ("div" | "mod") ~ ("(" ~> expr) ~ ("," ~> expr) <~ ")" ^^ {
    case "div" ~ x ~ y => x / y
    case "mod" ~ x ~ y => x % y
  }
  def function: Parser[BigInteger] = function1 | function2
  def base: Parser[BigInteger] = number | function | "(" ~> expr <~ ")"
  def unsignedFactor: Parser[BigInteger] = base ~ ((("**" | "^") ~> unsignedFactor)?) ^^ {
    case x ~ option => option match {
      case Some(exp) => pow(x, exp)
      case None => x
    }
  }
  def factor: Parser[BigInteger] = ("-"?) ~ unsignedFactor ^^ {
    case option ~ factor => option match {
      case Some(sign) => -factor
      case None => factor
    }
  }
  def unsignedTerm: Parser[BigInteger] = unsignedFactor ~ (("*" ~ factor)*) ^^ {
    case factor ~ list => (factor /: list) {
      case (x, "*" ~ y) => x * y
    }
  }
  def term: Parser[BigInteger] = ("-"?) ~ unsignedTerm ^^ {
    case option ~ term => option match {
      case Some(sign) => -term
      case None => term
    }
  }
  def expr: Parser[BigInteger] = term ~ (("+" ~ unsignedTerm | "-" ~ unsignedTerm)*) ^^ {
    case term ~ list => (term /: list) {
      case (x, "+" ~ y) => x + y
      case (x, "-" ~ y) => x - y
    }
  }
  def comparison: Parser[Boolean] = expr ~ ("=" | "<>" | "<=" | "<" | ">=" | ">") ~ expr ^^ {
    case x ~ "=" ~ y => x >< y
    case x ~ "<>" ~ y => x <> y
    case x ~ "<=" ~ y => x <= y
    case x ~ "<" ~ y => x < y
    case x ~ ">=" ~ y => x >= y
    case x ~ ">" ~ y => x > y
  }
}
