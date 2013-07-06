package scas.application

import scala.math
import scala.util.parsing.combinator.RegexParsers
import scas._

object Parsers extends RegexParsers {
  import Implicits.{ZZ, CC, infixOrderingOps, infixUFDOps, infixPowerProductOps}
  type Element = RationalFunction.Element[MultivariatePolynomial.Element[BigInteger, Int], BigInteger, Int]
  implicit var r = ring()
  var n = List.empty[String]

  def convert(x: Element) = if (x.factory == r) x else r.convert(x)

  def ring(variables: Variable*) = RationalFunction.integral(MultivariatePolynomial(ZZ, PowerProduct(variables: _*)))

  def bigInteger: Parser[BigInteger] = """\d+""".r ^^ { s: String => BigInteger(s) }
  def integer: Parser[Int] = """\d+""".r ^^ { _.toInt }
  def boolean: Parser[Boolean] = ("true" | "false") ^^ { _.toBoolean }
  def double: Parser[Double] = """\d+(\.\d*)?""".r ^^ { _.toDouble }
  def name: Parser[String] = """[a-zA-Z]+""".r
  def prime: Parser[Int] = """'*""".r ^^ { _.length }
  def subscript: Parser[Int] = "[" ~> integer <~ "]"
  def variable: Parser[Variable] = name ~ prime ~ (subscript*) ^^ {
    case name ~ prime ~ list => Variable(name, prime, list.toArray)
  }
  def constant: Parser[Double] = ("pi") ^^ {
    case "pi" => math.Pi
  }

  def number: Parser[Element] = bigInteger ^^ { value: BigInteger => value }
  def complex: Parser[Complex] = bigInteger ^^ { value: BigInteger => value }
  def function1Int: Parser[BigInteger] = ("factorial") ~ ("(" ~> exprInt) <~ ")" ^^ {
    case "factorial" ~ x if (x > BigInteger(0)) => BigInteger.factorial(x)
  }
  def function2Int: Parser[BigInteger] = ("div" | "mod") ~ ("(" ~> exprInt) ~ ("," ~> exprInt) <~ ")" ^^ {
    case "div" ~ x ~ y => x / y
    case "mod" ~ x ~ y => x % y
  }
  def functionInt: Parser[BigInteger] = function1Int | function2Int
  def functionComplex: Parser[Complex] = ("sqrt" | "real" | "imag" | "conjugate") ~ ("(" ~> exprComplex) <~ ")" ^^ {
    case "sqrt" ~ x if (x >< BigInteger(-1)) => Complex.sqrt(x)
    case "real" ~ x => Complex.realPart(x)
    case "imag" ~ x => Complex.imaginaryPart(x)
    case "conjugate" ~ x => Complex.conjugate(x)
  }
  def functionRF: Parser[Element] = ("factor") ~ ("(" ~> exprInt) <~ ")" ^^ {
    case "factor" ~ x if (x <> BigInteger(0)) => factor(x)
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
  def functionFn: Parser[Double => Double] = ("sin" | "cos" | "tan" | "exp" | "log" | "sqrt") ~ ("(" ~> exprFn) <~ ")" ^^ {
    case "sin" ~ x => { a: Double => math.sin(x(a)) }
    case "cos" ~ x => { a: Double => math.cos(x(a)) }
    case "tan" ~ x => { a: Double => math.tan(x(a)) }
    case "exp" ~ x => { a: Double => math.exp(x(a)) }
    case "log" ~ x => { a: Double => math.log(x(a)) }
    case "sqrt" ~ x => { a: Double => math.sqrt(x(a)) }
  }
  def constantFn: Parser[Double => Double] = (double | constant) ^^ { value: Double => { a: Double => value } }
  def generator: Parser[Element] = variable ^^ { s: Variable => generator(s) }
  def generator(s: Variable) = {
    val variables = r.variables
    if (variables.contains(s)) r.generator(variables.indexOf(s))
    else {
      r = ring(variables++Array(s): _*)
      r.generator(variables.length)
    }
  }
  def generatorFn: Parser[Double => Double] = name ^^ {
    case name if (contains(name)) => identity[Double]
  }
  def baseInt: Parser[BigInteger] = bigInteger | functionInt | "(" ~> exprInt <~ ")"
  def baseComplex: Parser[Complex] = complex | functionComplex | "(" ~> exprComplex <~ ")"
  def baseRF: Parser[Element] = number | functionRF | generator | "(" ~> exprRF <~ ")"
  def baseBoolean: Parser[Boolean] = boolean | "(" ~> exprBoolean <~ ")"
  def baseFn: Parser[Double => Double] = constantFn | functionFn | generatorFn | "(" ~> exprFn <~ ")"
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
  def unsignedFactorFn: Parser[Double => Double] = baseFn ~ ((("**" | "^") ~> unsignedFactorInt)?) ^^ {
    case x ~ option => option match {
      case Some(exp) => { a: Double => scala.math.pow(x(a), exp.doubleValue()) }
      case None => x
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
  def factorFn: Parser[Double => Double] = ("-"?) ~ unsignedFactorFn ^^ {
    case option ~ factor => option match {
      case Some(sign) => -factor(_: Double)
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
  def unsignedTermFn: Parser[Double => Double] = unsignedFactorFn ~ (("*" ~ factorFn | "/" ~ factorFn)*) ^^ {
    case factor ~ list => (factor /: list) {
      case (x, "*" ~ y) => { a: Double => x(a) * y(a) }
      case (x, "/" ~ y) => { a: Double => x(a) / y(a) }
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
  def termFn: Parser[Double => Double] = ("-"?) ~ unsignedTermFn ^^ {
    case option ~ term => option match {
      case Some(sign) => -term(_: Double)
      case None => term
    }
  }
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
  def exprFn: Parser[Double => Double] = termFn ~ (("+" ~ unsignedTermFn | "-" ~ unsignedTermFn)*) ^^ {
    case term ~ list => (term /: list) {
      case (x, "+" ~ y) => { a: Double => x(a) + y(a) }
      case (x, "-" ~ y) => { a: Double => x(a) - y(a) }
    }
  }
  def graph: Parser[Graph] = "graph" ~> ("(" ~> exprFn) ~ ("," ~> name) <~ ")" ^^ {
    case expr ~ name if (contains(name)) => Graph(expr)
  }
  def contains(name: String) = {
    if (n.isEmpty) n = List(name)
    n.contains(name)
  }

  def apply(input: String) = {
    val result = parseAll(exprInt, input) match {
      case Success(result, _) => Right(Some(result))
      case NoSuccess(msg, _) if (incomplete(msg)) => Right(None)
      case NoSuccess(msg, _) => parseAll(exprComplex, input) match {
        case Success(result, _) => Right(Some(result))
        case NoSuccess(msg, _) if (incomplete(msg)) => Right(None)
        case NoSuccess(msg, _) => parseAll(exprRF, input) match {
          case Success(result, _) => Right(Some(result))
          case NoSuccess(msg, _) if (incomplete(msg)) => Right(None)
          case NoSuccess(msg, _) => parseAll(exprBoolean, input) match {
            case Success(result, _) => Right(Some(java.lang.Boolean.valueOf(result)))
            case NoSuccess(msg, _) if (incomplete(msg)) => Right(None)
            case NoSuccess(msg, _) => parseAll(graph, input) match {
              case Success(result, _) => Right(Some(result))
              case NoSuccess(msg, _) if (incomplete(msg)) => Right(None)
              case NoSuccess(msg, _) => Left(msg)
            }
          }
        }
      }
    }
    r = ring()
    result
  }
  def incomplete(msg: String) = msg.endsWith("end of source found")
}
