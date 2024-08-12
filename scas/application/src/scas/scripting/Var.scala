package scas.scripting

import Parsers._
import scas.variable.Variable

object Var {
  def integer: Parser[Int] = """\d+""".r ^^ { _.toInt }
  def name: Parser[String] = """[a-zA-Z]+""".r
  def prime: Parser[Int] = """'*""".r ^^ { _.length }
  def subscript: Parser[Int] = "[" ~> integer <~ "]"
  def parser: Parser[Variable] = name ~ prime ~ rep(subscript) ^^ {
    case name ~ prime ~ list => Variable(name, prime, list*)
  }
}
