package scas.scripting

import Parsers._
import scala.annotation.nowarn
import scas.quotient.RationalFunction
import scas.structure.commutative.Quotient.Element
import scas.polynomial.PolynomialOverUFD
import scas.base.BigInteger

type RF = Element[Poly]

class RFParsers(using var structure: RationalFunction) extends UFDParsers[RF] {
  def this(ring: PolynomialOverUFD[Poly, BigInteger, Array[Int]]) = this(using new RationalFunction(using ring))
  val poly = new PolyParsers(using structure.ring)

  def base: Parser[RF] = poly.base ^^ {
    case x if (poly.structure == structure.ring) => structure(x)
    case x => {
      structure = new RationalFunction(using poly.structure)
      structure(x)
    }
  } | "(" ~> expr <~ ")"
  @nowarn("msg=match may not be exhaustive")
  override def unsignedTerm: Parser[RF] = unsignedFactor ~ rep(("*" | "/") ~ factor) ^^ {
    case factor ~ list => list.foldLeft(factor) {
      case (x, "*" ~ y) => x.convert * y.convert
      case (x, "/" ~ y) => x.convert / y.convert
    }
  }
  @nowarn("msg=match may not be exhaustive")
  override def expr: Parser[RF] = term ~ rep(("+" | "-") ~ unsignedTerm) ^^ {
    case term ~ list => list.foldLeft(term) {
      case (x, "+" ~ y) => x.convert + y.convert
      case (x, "-" ~ y) => x.convert - y.convert
    }
  }
  @nowarn("msg=match may not be exhaustive")
  override def comparison: Parser[Boolean] = expr ~ ("=" | "<>") ~ expr ^^ {
    case x ~ "=" ~ y => x.convert >< y.convert
    case x ~ "<>" ~ y => x.convert <> y.convert
  }

  def reset: Unit = {
    poly.reset
  }
}

object RFParsers extends RFParsers(Poly())
