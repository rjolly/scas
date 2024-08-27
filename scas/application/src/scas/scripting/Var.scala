package scas.scripting

import Parsers._
import scas.variable.Variable
import java.lang.Integer

object Var {
  def integer: Parser[Integer] = """\d+""".r ^^ { _.toInt }
  def name: Parser[String] = """[a-zA-Z]+""".r
  def prime: Parser[Integer] = """'*""".r ^^ { _.length }
  def subscript: Parser[Integer] = "[" ~> integer <~ "]"
  def parser: Parser[Variable] = name ~ prime ~ rep(subscript) ^^ {
    case name ~ prime ~ list => Variable(name, prime.intValue(), list.mapInt*)
  }
  extension (s: Seq[Integer]) def mapInt = s.foldLeft(Seq.empty[Int])((l, r) => l ++ Seq(r.intValue()))
}
