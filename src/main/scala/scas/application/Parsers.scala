package scas.application

object Parsers extends scala.util.parsing.combinator.RegexParsers {
  def integer: Parser[Int] = """\d+""".r ^^ { _.toInt }
  def boolean: Parser[Boolean] = ("true" | "false") ^^ { _.toBoolean }
  def double: Parser[Double] = """\d+(\.\d*)?""".r ^^ { _.toDouble }
  def name: Parser[String] = """[a-zA-Z]+""".r
  def prime: Parser[Int] = """'*""".r ^^ { _.length }
  def subscript: Parser[Int] = "[" ~> integer <~ "]"

  def expr: Parser[Object] = Fn.graph | (RF.expr ||| ComplexParsers.expr ||| (Boolean.expr ^^ { java.lang.Boolean.valueOf(_) }))

  def apply(input: String) = {
    val result = parseAll(expr, input) match {
      case Success(result, _) => Right(result)
      case NoSuccess(msg, _) => Left(msg)
    }
    RF.reset
    Fn.reset
    result
  }
}
