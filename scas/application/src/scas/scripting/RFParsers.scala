package scas.scripting

import Parsers._
import scas.quotient.RationalFunction
import scas.structure.commutative.Quotient.Element
import scas.polynomial.PolynomialOverUFD
import scas.base.BigInteger

type RF = Element[Poly]

class RFParsers(using var structure: RationalFunction) extends FieldParsers[RF] {
  def this(ring: PolynomialOverUFD[Poly, BigInteger, Array[Int]]) = this(using new RationalFunction(using ring))
  val poly = new PolyParsers(using structure.ring)

  def base: Parser[RF] = poly.base ^^ {
    case x if (poly.structure == structure.ring) => structure(x)
    case x => {
      structure = new RationalFunction(using poly.structure)
      structure(x)
    }
  } | "(" ~> expr <~ ")"

  def reset: Unit = {
    poly.reset
  }
}

object RFParsers extends RFParsers(Poly())
