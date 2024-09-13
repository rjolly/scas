package scas.scripting

import Parsers.{log => _, _}
import math3.Double
import Math.{sinh, cosh, tanh, sin, cos, tan, asin, acos, atan, exp, log, pow, PI}

object DoubleParsers extends FieldParsers[Double] {
  given structure: Double.Impl = Double
  def number: Parser[Double] = """(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?""".r ^^ { _.toDouble }
  def constant: Parser[Double] = ("pi") ^^ {
    case "pi" => PI
  }
  def function: Parser[Double] = ("sinh" | "cosh" | "tanh" | "sin" | "cos" | "tan" | "asin" | "acos" | "atan" | "exp" | "log") ~ ("(" ~> expr) <~ ")" ^^ {
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
  }
  def base: Parser[Double] = number | constant | function | "(" ~> expr <~ ")"
  override def unsignedFactor: Parser[Double] = base ~ opt(("**" | "^") ~> factor) ^^ {
    case x ~ option => option match {
      case Some(y) => pow(x, y)
      case None => x
    }
  }
}
