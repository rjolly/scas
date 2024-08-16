package scas.scripting

import Parsers.{log => _, _}
import scas.rendering.Graph
import scas.variable.Variable
import Function.{sinh, cosh, tanh, sin, cos, tan, asin, acos, atan, exp, log, sqrt, abs, pow, identity}

class Fn(var n: Variable*) extends UFDParsers[Double => Double] {
  given structure: Function.type = Function

  def function: Parser[Double => Double] = ("sinh" | "cosh" | "tanh" | "sin" | "cos" | "tan" | "asin" | "acos" | "atan" | "exp" | "log" | "sqrt" | "abs") ~ ("(" ~> expr) <~ ")" ^^ {
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
    case "abs" ~ x => abs(x)
  }
  def generator: Parser[Double => Double] = Var.parser ^^ {
    case variable if (contains(variable)) => identity
  }
  def base: Parser[Double => Double] = DoubleParsers.base ^^ { Function(_) } | function | generator | "(" ~> expr <~ ")"
  override def unsignedFactor: Parser[Double => Double] = base ~ opt(("**" | "^") ~> factor) ^^ {
    case x ~ option => option match {
      case Some(y) => pow(x, y)
      case None => x
    }
  }
  def graph: Parser[Graph] = "graph" ~> ("(" ~> expr) ~ ("," ~> Var.parser) <~ ")" ^^ {
    case expr ~ variable if (contains(variable)) => Graph(expr)
  }
  def contains(variable: Variable) = {
    if (n.isEmpty) n = List(variable)
    n.contains(variable)
  }
}
