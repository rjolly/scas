package scas.scripting

object Parsers extends scala.util.parsing.combinator.RegexParsers {
  def obj: Parser[Object] = {
    Fn().graph | (ComplexParsers.obj ||| DoubleParsers.obj ||| RFParsers(PolyParsers.newInstance()).obj ||| RationalParsers.obj ||| BooleanParsers.obj)
  }

  def apply(input: String) = {
    val result = parseAll(obj, input) match {
      case Success(result, _) => Right(result)
      case NoSuccess.I(msg, _) => Left(msg)
    }
    result
  }
}
