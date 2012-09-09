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

  def number: Parser[Either[Element, BigInteger]] = bigInteger ^^ { value: BigInteger => Right(value) }
  def function: Parser[Either[Element, BigInteger]] = name ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ {
    case name ~ list => list match {
      case Nil => Left(generator(name))
      case x::Nil => name match {
        case "sqrt" => x match {
          case Right(x) if (x >< BigInteger(-1)) => Left(sqrt(x))
        }
        case "factorial" => x match {
          case Right(x) if (x > BigInteger(0)) => Right(BigInteger.factorial(x))
        }
        case "factor" => x match {
          case Right(x) if (x >< BigInteger(0)) => Right(0)
          case Right(x) => Left(factor(x))
        }
      }
      case x::y::Nil => name match {
        case "div" => x match {
          case Right(x) => y match {
            case Right(y) => Right(x / y)
            case _ => throw new RuntimeException
          }
          case _ => throw new RuntimeException
        }
        case "mod" => x match {
          case Right(x) => y match {
            case Right(y) => Right(x % y)
            case _ => throw new RuntimeException
          }
          case _ => throw new RuntimeException
        }
      }
      case _ => throw new RuntimeException
    }
  }
  def factor(x: BigInteger): Element = {
    val map = BigInteger.factor(BigInteger.abs(x))
    val s = map.keys.toArray.map({ x: BigInteger => Variable(x.toString) })
    val variables = r.variables.union(s).distinct
    if (variables.length > r.variables.length) r = ring(variables: _*)
    implicit val p = r.ring.pp
    val m = (p.one /: map) { case (l, (a, b)) =>
      val x = p.generator(Variable(a.toString))
      l * pow(x, b)
    }
    r(r.ring(BigInteger.signum(x))*r.ring.fromPowerProduct(m))
  }
  def generator: Parser[Either[Element, BigInteger]] = variable ^^ {
    s: Variable => Left(generator(s))
  }
  def generator(s: Variable): Element = {
    val variables = r.variables
    if (variables.contains(s)) r.generator(variables.indexOf(s))
    else {
      r = ring(variables++Array(s): _*)
      r.generator(variables.length)
    }
  }
  def base: Parser[Either[Element, BigInteger]] = number | function | generator | "(" ~> expr <~ ")"
  def unsignedFactor: Parser[Either[Element, BigInteger]] = base ~ ((("**" | "^") ~> base)*) ^^ {
    case base ~ list => base::list reduceRight {
      (x: Either[Element, BigInteger], exp: Either[Element, BigInteger]) => x match {
        case Left(x) => exp match {
          case Right(exp) => Left(pow(convert(x), exp))
          case _ => throw new RuntimeException
        }
        case Right(x) => exp match {
          case Right(exp) if (exp >= BigInteger(0)) => Right(pow(x, exp))
        }
      }
    }
  }
  def factor: Parser[Either[Element, BigInteger]] = ("-"?) ~ unsignedFactor ^^ {
    case option ~ factor => option match {
      case Some(sign) => factor match {
        case Left(factor) => Left(-factor)
        case Right(factor) => Right(-factor)
      }
      case None => factor
    }
  }
  def unsignedTerm: Parser[Either[Element, BigInteger]] = unsignedFactor ~ (("*" ~ factor | "/" ~ factor | "%" ~ factor)*) ^^ {
    case factor ~ list => (factor /: list) {
      case (x, "*" ~ y) => x match {
        case Left(x) => y match {
          case Left(y) => Left(convert(x) * convert(y))
          case Right(y) => Left(convert(x) * convert(y))
        }
        case Right(x) => y match {
          case Left(y) => Left(convert(x) * convert(y))
          case Right(y) => Right(x * y)
        }
      }
      case (x, "/" ~ y) => x match {
        case Left(x) => y match {
          case Left(y) => Left(convert(x) / convert(y))
          case Right(y) => Left(convert(x) / convert(y))
        }
        case Right(x) => y match {
          case Left(y) => Left(convert(x) / convert(y))
          case Right(y) => Left(frac(x, y))
        }
      }
    }
  }
  def term: Parser[Either[Element, BigInteger]] = ("-"?) ~ unsignedTerm ^^ {
    case option ~ term => option match {
      case Some(sign) => term match {
        case Left(term) => Left(-term)
        case Right(term) => Right(-term)
      }
      case None => term
    }
  }
  def expr: Parser[Either[Element, BigInteger]] = term ~ (("+" ~ unsignedTerm | "-" ~ unsignedTerm)*) ^^ {
    case term ~ list => (term /: list) {
      case (x, "+" ~ y) => x match {
        case Left(x) => y match {
          case Left(y) => Left(convert(x) + convert(y))
          case Right(y) => Left(convert(x) + convert(y))
        }
        case Right(x) => y match {
          case Left(y) => Left(convert(x) + convert(y))
          case Right(y) => Right(x + y)
        }
      }
      case (x, "-" ~ y) => x match {
        case Left(x) => y match {
          case Left(y) => Left(convert(x) - convert(y))
          case Right(y) => Left(convert(x) - convert(y))
        }
        case Right(x) => y match {
          case Left(y) => Left(convert(x) - convert(y))
          case Right(y) => Right(x - y)
        }
      }
    }
  }

  def apply(input: String): Either[String, Element] = {
    r = ring()
    parseAll(expr, input) match {
      case Success(Left(result), _) => Right(result)
      case Success(Right(result), _) => Right(result)
      case NoSuccess(msg, _) => Left(msg)
    }
  }
}
