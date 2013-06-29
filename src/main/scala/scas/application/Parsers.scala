package scas.application

import scala.util.parsing.combinator.RegexParsers
import scas._

object Parsers extends RegexParsers {
  import Implicits.{ZZ, CC, infixOrderingOps, infixUFDOps, infixPowerProductOps}
  type Element = RationalFunction.Element[MultivariatePolynomial.Element[BigInteger, Int], BigInteger, Int]
  implicit var r = ring()

  def convert(x: Element) = if (x.factory == r) x else r.convert(x)

  def ring(variables: Variable*) = RationalFunction.integral(MultivariatePolynomial(ZZ, PowerProduct(variables: _*)))

  def bigInteger: Parser[BigInteger] = """[0-9]+""".r ^^ { s: String => BigInteger(s) }
  def integer: Parser[Int] = """[0-9]+""".r ^^ { _.toInt }
  def boolean: Parser[Boolean] = "true" ^^^ true | "false" ^^^ false
  def name: Parser[String] = """[a-zA-Z]+""".r
  def prime: Parser[Int] = """'*""".r ^^ { _.length }
  def subscript: Parser[Int] = "[" ~> integer <~ "]"
  def variable: Parser[Variable] = name ~ prime ~ (subscript*) ^^ {
    case name ~ prime ~ list => Variable(name, prime, list.toArray)
  }

  def number: Parser[Element] = bigInteger ^^ { value: BigInteger => value }
  def complex: Parser[Complex] = bigInteger ^^ { value: BigInteger => value }
  def factorial: Parser[BigInteger] = "factorial" ~> ("(" ~> exprInt) <~ ")" ^^ {
    case x if (x > BigInteger(0)) => BigInteger.factorial(x)
  }
  def integral: Parser[BigInteger] = ("div" | "mod") ~ ("(" ~> exprInt) ~ ("," ~> exprInt) <~ ")" ^^ {
    case "div" ~ x ~ y => x / y
    case "mod" ~ x ~ y => x % y
  }
  def functionInt: Parser[BigInteger] = factorial | integral
  def sqrt: Parser[Complex] = "sqrt" ~> ("(" ~> exprInt) <~ ")" ^^ {
    case x if (x >< BigInteger(-1)) => Complex.sqrt(x)
  }
  def part: Parser[Complex] = ("real" | "imag" | "conjugate") ~ ("(" ~> exprComplex) <~ ")" ^^ {
    case "real" ~ x => Complex.realPart(x)
    case "imag" ~ x => Complex.imaginaryPart(x)
    case "conjugate" ~ x => Complex.conjugate(x)
  }
  def functionComplex: Parser[Complex] = sqrt | part
  def factor: Parser[Element] = "factor" ~> ("(" ~> exprInt) <~ ")" ^^ {
    case x if (x <> BigInteger(0)) => factor(x)
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
  def functionRF: Parser[Element] = factor
  def comparisonInt: Parser[Boolean] = exprInt ~ ("=" | "<>" | "<=" | "<" | ">=" | ">") ~ exprInt ^^ {
    case x ~ "=" ~ y => x >< y
    case x ~ "<>" ~ y => x <> y
    case x ~ "<=" ~ y => x <= y
    case x ~ "<" ~ y => x < y
    case x ~ ">=" ~ y => x >= y
    case x ~ ">" ~ y => x > y
  }
  def comparisonComplex: Parser[Boolean] = exprComplex ~ ("=" | "<>") ~ exprComplex ^^ {
    case x ~ "=" ~ y => x >< y
    case x ~ "<>" ~ y => x <> y
  }
  def comparisonRF: Parser[Boolean] = exprRF ~ ("=" | "<>") ~ exprRF ^^ {
    case x ~ "=" ~ y => x >< y
    case x ~ "<>" ~ y => x <> y
  }
  def comparison: Parser[Boolean] = baseBoolean ~ ("=>" | "=" | "<>") ~ baseBoolean ^^ {
    case x ~ "=>" ~ y => y || !x
    case x ~ "=" ~ y => x == y
    case x ~ "<>" ~ y => x != y
  }
  def negation: Parser[Boolean] = "!" ~> baseBoolean ^^ { case x => !x }
  def functionBoolean: Parser[Boolean] = comparisonInt | comparisonComplex | comparisonRF | comparison | negation
  def generator: Parser[Element] = variable ^^ { s: Variable => generator(s) }
  def generator(s: Variable) = {
    val variables = r.variables
    if (variables.contains(s)) r.generator(variables.indexOf(s))
    else {
      r = ring(variables++Array(s): _*)
      r.generator(variables.length)
    }
  }
  def baseInt: Parser[BigInteger] = bigInteger | functionInt | "(" ~> exprInt <~ ")"
  def baseComplex: Parser[Complex] = complex | functionComplex | "(" ~> exprComplex <~ ")"
  def baseRF: Parser[Element] = number | functionRF | generator | "(" ~> exprRF <~ ")"
  def baseBoolean: Parser[Boolean] = boolean | "(" ~> exprBoolean <~ ")"
  def unsignedFactorInt: Parser[BigInteger] = baseInt ~ ((("**" | "^") ~> baseInt)*) ^^ {
    case base ~ list => base::list reduceRight {
      (x: BigInteger, exp: BigInteger) => pow(x, exp)
    }
  }
  def unsignedFactorComplex: Parser[Complex] = baseComplex ~ ((("**" | "^") ~> unsignedFactorInt)?) ^^ {
    case x ~ option => option match {
      case Some(exp) => pow(x, exp)
      case None => x
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
  def factorComplex: Parser[Complex] = ("-"?) ~ unsignedFactorComplex ^^ {
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
  def unsignedTermComplex: Parser[Complex] = unsignedFactorComplex ~ (("*" ~ factorComplex | "/" ~ factorComplex)*) ^^ {
    case factor ~ list => (factor /: list) {
      case (x, "*" ~ y) => x * y
      case (x, "/" ~ y) => x / y
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
  def termComplex: Parser[Complex] = ("-"?) ~ unsignedTermComplex ^^ {
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
  def termBoolean: Parser[Boolean] = functionBoolean | baseBoolean
  def exprInt: Parser[BigInteger] = termInt ~ (("+" ~ unsignedTermInt | "-" ~ unsignedTermInt)*) ^^ {
    case term ~ list => (term /: list) {
      case (x, "+" ~ y) => x + y
      case (x, "-" ~ y) => x - y
    }
  }
  def exprComplex: Parser[Complex] = termComplex ~ (("+" ~ unsignedTermComplex | "-" ~ unsignedTermComplex)*) ^^ {
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
  def exprBoolean: Parser[Boolean] = termBoolean ~ (("&" ~ termBoolean | "|" ~ termBoolean | "^" ~ termBoolean)*) ^^ {
    case term ~ list => (term /: list) {
      case (x, "&" ~ y) => x && y
      case (x, "|" ~ y) => x || y
      case (x, "^" ~ y) => x ^ y
    }
  }

  def apply(input: String) = {
    val result = parseAll(exprInt, input) match {
      case Success(result, _) => Right(result)
      case NoSuccess(msg, _) => parseAll(exprComplex, input) match {
        case Success(result, _) => Right(result)
        case NoSuccess(msg, _) => parseAll(exprRF, input) match {
          case Success(result, _) => Right(result)
          case NoSuccess(msg, _) => parseAll(exprBoolean, input) match {
            case Success(result, _) => Right(java.lang.Boolean.valueOf(result))
            case NoSuccess(msg, _) => Left(msg)
          }
        }
      }
    }
    r = ring()
    result
  }
}
