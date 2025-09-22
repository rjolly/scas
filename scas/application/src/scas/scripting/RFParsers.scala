package scas.scripting

import Parsers.*
import scas.quotient.RationalFunction
import scas.structure.commutative.Quotient.Element
import scas.polynomial.ufd.PolynomialOverUFD
import scas.base.BigInteger

type RF = Element[Poly]

class RFParsers(var using_structure: RationalFunction) extends FieldParsers[RF] {
  def this(ring: PolynomialOverUFD[Poly, BigInteger, Array[Int]]) = this(new RationalFunction(using ring))
  override given structure: () => RationalFunction = using_structure
  val poly = new PolyParsers(structure.ring)

  def base: Parser[RF] = poly.base ^^ {
    case x if (poly.structure == structure.ring) => structure(x)
    case x => {
      using_structure = new RationalFunction(using poly.structure)
      structure(x)
    }
  } | "(" ~> expr <~ ")"

  def reset: Unit = {
    poly.reset
  }
}

object RFParsers extends RFParsers(Poly())
