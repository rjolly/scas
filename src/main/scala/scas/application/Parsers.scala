package scas.application

object Parsers extends scala.util.parsing.combinator.RegexParsers {
  def obj: Parser[Object] = Fn.graph | (ComplexParsers.obj ||| RF.obj ||| RationalParsers.obj ||| Boolean.obj)

  def apply(input: String) = {
    val result = parseAll(obj, input) match {
      case Success(result, _) => Right(result)
      case NoSuccess(msg, _) => Left(msg)
    }
    RF.reset
    Fn.reset
    result
  }
}
