package scas.application

import scala.util.parsing.combinator.RegexParsers
import scas._

object Parsers extends RegexParsers {
  import Implicits.{ZZ, CC, infixOrderingOps, infixUFDOps, infixPowerProductOps}
  type Element = RationalFunction.Element[MultivariatePolynomial.Element[Complex, Int], Complex, Int]
  implicit var r = ring()

  def convert(x: Element) = if (x.factory == r) x else r.convert(x)

  def ring(variables: Variable*) = RationalFunction(MultivariatePolynomial(CC, PowerProduct(variables: _*)))

  def bigInteger: Parser[BigInteger] = """[0-9]+""".r ^^ { s: String => BigInteger(s) }
  def integer: Parser[Int] = """[0-9]+""".r ^^ { _.toInt }
  def name: Parser[String] = """[a-zA-Z]+""".r
  def prime: Parser[Int] = """'*""".r ^^ { _.length }
  def subscript: Parser[Int] = "[" ~> integer <~ "]"
  def variable: Parser[Variable] = name ~ prime ~ (subscript*) ^^ {
    case name ~ prime ~ list => Variable(name, prime, list.toArray)
  }

  def number: Parser[Element] = bigInteger ^^ { value: BigInteger => value }
  def functionIntInt: Parser[BigInteger] = name ~ ("(" ~> repsep(exprInt, ",") <~ ")") ^^ {
    case name ~ list => list match {
      case x::Nil => name match {
        case "factorial" if (x > BigInteger(0)) => BigInteger.factorial(x)
      }
      case x::y::Nil => name match {
        case "div" => x / y
        case "mod" => x % y
      }
      case _ => throw new RuntimeException
    }
  }
  def functionIntRF: Parser[Element] = name ~ ("(" ~> repsep(exprInt, ",") <~ ")") ^^ {
    case name ~ list => list match {
      case Nil => generator(name)
      case x::Nil => name match {
        case "sqrt" if (x >< BigInteger(-1)) => sqrt(x)
        case "factor" if (x <> BigInteger(0)) => factor(x)
      }
      case _ => throw new RuntimeException
    }
  }
  def factor(x: BigInteger) = {
    val map = BigInteger.factor(BigInteger.abs(x))
    val s = map.keys.toArray.map({ x: BigInteger => Variable(x.toString) })
    val variables = r.variables.union(s).distinct
    if (variables.length > r.variables.length) r = ring(variables: _*)
    implicit val p = r.ring.pp
    val m = (p.one /: map) { case (l, (a, b)) =>
      val x = p.generator(Variable(a.toString))
      l * pow(x, b)
    }
    r(r.ring(m, BigInteger.signum(x)))
  }
  def generator: Parser[Element] = variable ^^ { s: Variable => generator(s) }
  def generator(s: Variable) = {
    val variables = r.variables
    if (variables.contains(s)) r.generator(variables.indexOf(s))
    else {
      r = ring(variables++Array(s): _*)
      r.generator(variables.length)
    }
  }
  def baseInt: Parser[BigInteger] = bigInteger | functionIntInt | "(" ~> exprInt <~ ")"
  def baseRF: Parser[Element] = number | functionIntRF | generator | "(" ~> exprRF <~ ")"
  def unsignedFactorInt: Parser[BigInteger] = baseInt ~ ((("**" | "^") ~> baseInt)*) ^^ {
    case base ~ list => base::list reduceRight {
      (x: BigInteger, exp: BigInteger) => pow(x, exp)
    }
  }
  def unsignedFactorRF: Parser[Element] = baseRF ~ ((("**" | "^") ~> unsignedFactorInt)?) ^^ {
    case x ~ option => option match {
      case Some(exp) => pow(convert(x), exp)
      case None => convert(x)
    }
  }
  def factorInt: Parser[BigInteger] = ("-"?) ~ unsignedFactorInt ^^ {
    case option ~ factor => option match {
      case Some(sign) => -factor
      case None => factor
    }
  }
  def factorRF: Parser[Element] = ("-"?) ~ unsignedFactorRF ^^ {
    case option ~ factor => option match {
      case Some(sign) => -factor
      case None => factor
    }
  }
  def unsignedTermInt: Parser[BigInteger] = unsignedFactorInt ~ (("*" ~ factorInt)*) ^^ {
    case factor ~ list => (factor /: list) {
      case (x, "*" ~ y) => x * y
    }
  }
  def unsignedTermRF: Parser[Element] = unsignedFactorRF ~ (("*" ~ factorRF | "/" ~ factorRF)*) ^^ {
    case factor ~ list => (factor /: list) {
      case (x, "*" ~ y) => convert(x) * convert(y)
      case (x, "/" ~ y) => convert(x) / convert(y)
    }
  }
  def termInt: Parser[BigInteger] = ("-"?) ~ unsignedTermInt ^^ {
    case option ~ term => option match {
      case Some(sign) => -term
      case None => term
    }
  }
  def termRF: Parser[Element] = ("-"?) ~ unsignedTermRF ^^ {
    case option ~ term => option match {
      case Some(sign) => -term
      case None => term
    }
  }
  def exprInt: Parser[BigInteger] = termInt ~ (("+" ~ unsignedTermInt | "-" ~ unsignedTermInt)*) ^^ {
    case term ~ list => (term /: list) {
      case (x, "+" ~ y) => x + y
      case (x, "-" ~ y) => x - y
    }
  }
  def exprRF: Parser[Element] = termRF ~ (("+" ~ unsignedTermRF | "-" ~ unsignedTermRF)*) ^^ {
    case term ~ list => (term /: list) {
      case (x, "+" ~ y) => convert(x) + convert(y)
      case (x, "-" ~ y) => convert(x) - convert(y)
    }
  }
  def expr: Parser[Object] = exprRF | exprInt

  def apply(input: String) = {
    val result = parseAll(expr, input) match {
      case Success(result, _) => Right(result)
      case NoSuccess(msg, _) => Left(msg)
    }
    r = ring()
    result
  }
}
