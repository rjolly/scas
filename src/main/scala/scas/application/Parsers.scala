package scas.application

object Parsers extends scala.util.parsing.combinator.RegexParsers {
  def obj: Parser[Object] = {
    val rf = new RFParsers
    (new Fn).graph | (ComplexParsers.obj ||| DoubleParsers.obj ||| rf.obj ||| RationalParsers.obj ||| (new BooleanParsers(rf)).obj)
  }

  def apply(input: String) = {
    val result = parseAll(obj, input) match {
      case Success(result, _) => Right(result)
      case NoSuccess(msg, _) => Left(msg)
    }
    result
  }
}
