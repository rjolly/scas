package scas.application

import java.lang.Math
import scas.Graph
import Parsers._

object Fn {
  import Math.{sinh, cosh, tanh, sin, cos, tan, asin, acos, atan, exp, log => ln, sqrt, pow, PI}

  var n = List.empty[String]

  def constant: Parser[Double] = ("pi") ^^ {
    case "pi" => PI
  }

  def reset = {
    n = List.empty[String]
  }

  def number: Parser[Double => Double] = (double | constant) ^^ { value: Double => { a: Double => value } }
  def function: Parser[Double => Double] = ("sinh" | "cosh" | "tanh" | "sin" | "cos" | "tan" | "asin" | "acos" | "atan" | "exp" | "log" | "sqrt") ~ ("(" ~> expr) <~ ")" ^^ {
    case "sinh" ~ x => { a: Double => sinh(x(a)) }
    case "cosh" ~ x => { a: Double => cosh(x(a)) }
    case "tanh" ~ x => { a: Double => tanh(x(a)) }
    case "sin" ~ x => { a: Double => sin(x(a)) }
    case "cos" ~ x => { a: Double => cos(x(a)) }
    case "tan" ~ x => { a: Double => tan(x(a)) }
    case "asin" ~ x => { a: Double => asin(x(a)) }
    case "acos" ~ x => { a: Double => acos(x(a)) }
    case "atan" ~ x => { a: Double => atan(x(a)) }
    case "exp" ~ x => { a: Double => exp(x(a)) }
    case "log" ~ x => { a: Double => ln(x(a)) }
    case "sqrt" ~ x => { a: Double => sqrt(x(a)) }
  }
  def generator: Parser[Double => Double] = name ^^ {
    case name if (contains(name)) => identity[Double]
  }
  def base: Parser[Double => Double] = number | function | generator | "(" ~> expr <~ ")"
  def unsignedFactor: Parser[Double => Double] = base ~ ((("**" | "^") ~> Int.unsignedFactor)?) ^^ {
    case x ~ option => option match {
      case Some(exp) => { a: Double => pow(x(a), exp.doubleValue()) }
      case None => x
    }
  }
  def factor: Parser[Double => Double] = ("-"?) ~ unsignedFactor ^^ {
    case option ~ factor => option match {
      case Some(sign) => -factor(_: Double)
      case None => factor
    }
  }
  def unsignedTerm: Parser[Double => Double] = unsignedFactor ~ (("*" ~ factor | "/" ~ factor)*) ^^ {
    case factor ~ list => (factor /: list) {
      case (x, "*" ~ y) => { a: Double => x(a) * y(a) }
      case (x, "/" ~ y) => { a: Double => x(a) / y(a) }
    }
  }
  def term: Parser[Double => Double] = ("-"?) ~ unsignedTerm ^^ {
    case option ~ term => option match {
      case Some(sign) => -term(_: Double)
      case None => term
    }
  }
  def expr: Parser[Double => Double] = term ~ (("+" ~ unsignedTerm | "-" ~ unsignedTerm)*) ^^ {
    case term ~ list => (term /: list) {
      case (x, "+" ~ y) => { a: Double => x(a) + y(a) }
      case (x, "-" ~ y) => { a: Double => x(a) - y(a) }
    }
  }
  def graph: Parser[Graph] = "graph" ~> ("(" ~> expr) ~ ("," ~> name) <~ ")" ^^ {
    case expr ~ name if (contains(name)) => Graph(expr)
  }
  def contains(name: String) = {
    if (n.isEmpty) n = List(name)
    n.contains(name)
  }
}
