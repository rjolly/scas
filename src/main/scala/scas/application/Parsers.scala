package scas.application

import scala.util.parsing.combinator.RegexParsers
import scas._

object Parsers extends RegexParsers {
  import Implicits.{ZZ, CC, infixAbelianGroupOps}
  type Element = RationalFunction.Element[
    MultivariatePolynomial.Element[
      Residue.Element[
        UnivariatePolynomial.Element[
          (java.math.BigInteger, java.math.BigInteger),
          Int
        ],
        (java.math.BigInteger, java.math.BigInteger),
        Int
      ],
      Int
    ],
    Residue.Element[
      UnivariatePolynomial.Element[
        (java.math.BigInteger, java.math.BigInteger),
        Int
      ],
      (java.math.BigInteger, java.math.BigInteger),
      Int
    ],
    Int
  ]
  implicit var r = ring()

  def convert(x: Element) = if (x.factory == r) x else r.convert(x)

  def ring(variables: Variable*) = RationalFunction(MultivariatePolynomial(CC, PowerProduct(variables: _*)))

  def integer: Parser[Int] = """[0-9]+""".r ^^ { _.toInt }
  def bigInteger: Parser[java.math.BigInteger] = """[0-9]+""".r ^^ { s: String => BigInteger(s) }
  def signedInteger: Parser[java.math.BigInteger] = ("-"?) ~ bigInteger ^^ {
    case option ~ integer => option match {
      case Some(sign) => -integer
      case _ => integer
    }
  }
  def unsignedExponent: Parser[java.math.BigInteger] = bigInteger ~ ((("**" | "^") ~> bigInteger)*) ^^ {
    case integer ~ list => integer::list reduceRight {
      (x: java.math.BigInteger, y: java.math.BigInteger) => pow(x, y)
    }
  }
  def exponent: Parser[java.math.BigInteger] = ("-"?) ~ unsignedExponent ^^ {
    case option ~ exponent => option match {
      case Some(sign) => -exponent
      case _ => exponent
    }
  }
  def number: Parser[Element] = bigInteger ^^ { value: java.math.BigInteger => r(value) }
  def name: Parser[String] = """[a-zA-Z]+""".r
  def prime: Parser[Int] = """'*""".r ^^ { _.length }
  def subscript: Parser[Int] = "[" ~> integer <~ "]"
  def variable: Parser[Variable] = name ~ prime ~ (subscript*) ^^ {
    case name ~ prime ~ list => Variable(name, prime, list.toArray)
  }
  def integerFunction: Parser[Element] = name ~ ("(" ~> bigInteger <~ ")") ^^ {
    case "factorial" ~ x => r(BigInteger.factorial(x))
    case "factor" ~ x => {
      val map = BigInteger.factor(x)
      val s = map.keys.map({ x: java.math.BigInteger => Variable(x.toString) }).toArray
      val variables = r.variables.union(s).distinct
      if (variables.length > r.variables.length) r = ring(variables: _*)
      (r.one /: map) { (l, p) =>
        val (a, b) = p
        val x = r.generator(variables.indexOf(Variable(a.toString)))
        l * pow(x, b)
      }
    }
  }
  def function: Parser[Element] = name ~ ("(" ~> signedInteger <~ ")") ^^ {
    case "sqrt" ~ x if (x >< -1) => r(sqrt(x))
  }
  def generator: Parser[Element] = variable ^^ { s: Variable =>
    val variables = r.variables
    if (variables.contains(s)) r.generator(variables.indexOf(s))
    else {
      r = ring(variables++Array(s): _*)
      r.generator(variables.length)
    }
  }
  def base: Parser[Element] = number | integerFunction | function | generator | "(" ~> expr <~ ")"
  def unsignedFactor: Parser[Element] = base ~ ((("**" | "^") ~> exponent)?) ^^ {
    case base ~ option => option match {
      case Some(exponent) => pow(convert(base), exponent)
      case _ => base
    }
  }
  def factor: Parser[Element] = ("-"?) ~ unsignedFactor ^^ {
    case option ~ factor => option match {
      case Some(sign) => -factor
      case _ => factor
    }
  }
  def unsignedTerm: Parser[Element] = unsignedFactor ~ (("*" ~ factor | "/" ~ factor | "%" ~ factor)*) ^^ {
    case factor ~ list => (convert(factor) /: list) {
      case (x, "*" ~ y) => x * convert(y)
      case (x, "/" ~ y) => x / convert(y)
      case (x, "%" ~ y) => x % convert(y)
    }
  }
  def term: Parser[Element] = ("-"?) ~ unsignedTerm ^^ {
    case option ~ term => option match {
      case Some(sign) => -term
      case _ => term
    }
  }
  def expr: Parser[Element] = term ~ (("+" ~ unsignedTerm | "-" ~ unsignedTerm)*) ^^ {
    case term ~ list => (convert(term) /: list) {
      case (x, "+" ~ y) => x + convert(y)
      case (x, "-" ~ y) => x - convert(y)
    }
  }

  def apply(input: String): Either[String, Element] = {
    r = ring()
    parseAll(expr, input) match {
      case Success(result, _) => Right(result)
      case NoSuccess(msg, _) => Left(msg)
    }
  }
}
