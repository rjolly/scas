package scas.scripting

object Parsers extends scala.util.parsing.combinator.RegexParsers {
  def obj: Parser[Object] = {
    val rf = RFParsers(false)
    val ba = BAParsers(b)
    val b = BooleanParsers(rf, ba)
    Fn().graph | (Int.obj ||| FactorParsers.obj ||| RationalParsers.obj ||| ComplexParsers.obj ||| DoubleParsers.obj ||| rf.obj ||| ba.obj ||| b.obj)
  }

  def apply(input: String) = {
    val result = parseAll(obj, input) match {
      case Success(result, _) => Right(result)
      case NoSuccess.I(msg, _) => Left(msg)
    }
    result
  }
}
