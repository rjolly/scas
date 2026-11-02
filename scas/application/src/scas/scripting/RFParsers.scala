package scas.scripting

import Parsers.*
import scas.quotient.growable.RationalFunction
import scas.structure.commutative.Quotient.Element
import scas.polynomial.growable.ufd.PolynomialOverUFD
import scas.base.BigInteger

type RF = Element[Poly]

class RFParsers(using RationalFunction[Int]) extends FieldParsers[RF] {
  def this(ring: PolynomialOverUFD[Poly, BigInteger, Array[Int]]) = this(using new RationalFunction(using summon, summon, ring))
  def this(dummy: Boolean) = this(Poly())
  override given structure: RationalFunction[Int] = summon
  val poly = new PolyParsers(using structure.ring)

  def base: Parser[RF] = poly.base ^^ {
    case x => structure(x)
  } | "(" ~> expr <~ ")"
}
