package scas.scripting

import Parsers.*
import scas.quotient.RationalFunction
import scas.structure.commutative.Quotient.Element
import scas.polynomial.ufd.growable.PolynomialOverUFD
import scas.base.BigInteger

type RF = Element[Poly]

class RFParsers(using RationalFunction) extends FieldParsers[RF] {
  def this(ring: PolynomialOverUFD[Poly, BigInteger, Array[Int]]) = this(new RationalFunction(using ring))
  override given structure: RationalFunction = summon
  val poly = new PolyParsers(using structure.ring)

  def base: Parser[RF] = poly.base ^^ {
    case x => structure(x)
  } | "(" ~> expr <~ ")"
}

object RFParsers extends RFParsers(Poly())
