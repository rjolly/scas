package scas.application

import Parsers.{log => _, _}
import scas.base.Function
import Function.{sinh, cosh, tanh, sin, cos, tan, asin, acos, atan, exp, log, sqrt, pow}
import scas.Graph

object Fn extends UFDParsers[Double => Double] {
  val structure = Function

  var n = List.empty[String]

  def reset = {
    n = List.empty[String]
  }

  def function: Parser[Double => Double] = ("sinh" | "cosh" | "tanh" | "sin" | "cos" | "tan" | "asin" | "acos" | "atan" | "exp" | "log" | "sqrt") ~ ("(" ~> expr) <~ ")" ^^ {
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
    case "log" ~ x => log(x)
    case "sqrt" ~ x => sqrt(x)
  }
  def generator: Parser[Double => Double] = name ^^ {
    case name if (contains(name)) => identity[Double]
  }
  def base: Parser[Double => Double] = Double.base ^^ { Function(_) } | function | generator | "(" ~> expr <~ ")"
  override def unsignedFactor: Parser[Double => Double] = base ~ ((("**" | "^") ~> factor)?) ^^ {
    case x ~ option => option match {
      case Some(y) => pow(x, y)
      case None => x
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
