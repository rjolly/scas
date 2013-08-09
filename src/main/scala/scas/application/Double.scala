package scas.application

import java.lang.Math
import Parsers._

object Double {
  import Math.{sinh, cosh, tanh, sin, cos, tan, asin, acos, atan, exp, log => ln, sqrt, pow, PI}

  def number: Parser[Double] = """(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?""".r ^^ { _.toDouble }
  def constant: Parser[Double] = ("pi") ^^ {
    case "pi" => PI
  }
  def function: Parser[Double] = ("sinh" | "cosh" | "tanh" | "sin" | "cos" | "tan" | "asin" | "acos" | "atan" | "exp" | "log" | "sqrt") ~ ("(" ~> expr) <~ ")" ^^ {
    case "sinh" ~ x => sinh(x)
    case "cosh" ~ x => cosh(x)
    case "tanh" ~ x => tanh(x)
    case "sin" ~ x => sin(x)
    case "cos" ~ x => cos(x)
    case "tan" ~ x => tan(x)
    case "asin" ~ x => asin(x)
    case "acos" ~ x => acos(x)
    case "atan" ~ x => atan(x)
    case "exp" ~ x => exp(x)
    case "log" ~ x => ln(x)
    case "sqrt" ~ x => sqrt(x)
  }
  def base: Parser[Double] = number | constant | function | "(" ~> expr <~ ")"
  def unsignedFactor: Parser[Double] = base ~ ((("**" | "^") ~> factor)?) ^^ {
    case x ~ option => option match {
      case Some(y) => pow(x, y)
      case None => x
    }
  }
  def factor: Parser[Double] = ("-"?) ~ unsignedFactor ^^ {
    case option ~ factor => option match {
      case Some(sign) => -factor
      case None => factor
    }
  }
  def unsignedTerm: Parser[Double] = unsignedFactor ~ (("*" ~ factor | "/" ~ factor)*) ^^ {
    case factor ~ list => (factor /: list) {
      case (x, "*" ~ y) => x * y
      case (x, "/" ~ y) => x / y
    }
  }
  def term: Parser[Double] = ("-"?) ~ unsignedTerm ^^ {
    case option ~ term => option match {
      case Some(sign) => -term
      case None => term
    }
  }
  def expr: Parser[Double] = term ~ (("+" ~ unsignedTerm | "-" ~ unsignedTerm)*) ^^ {
    case term ~ list => (term /: list) {
      case (x, "+" ~ y) => x + y
      case (x, "-" ~ y) => x - y
    }
  }
  def comparison: Parser[Boolean] = expr ~ ("=" | "<>" | "<=" | "<" | ">=" | ">") ~ expr ^^ {
    case x ~ "=" ~ y => x != y
    case x ~ "<>" ~ y => x == y
    case x ~ "<=" ~ y => x <= y
    case x ~ "<" ~ y => x < y
    case x ~ ">=" ~ y => x >= y
    case x ~ ">" ~ y => x > y
  }
}
