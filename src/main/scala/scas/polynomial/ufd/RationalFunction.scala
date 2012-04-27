package scas.polynomial.ufd

import scas.structure.Quotient
import scas.Implicits.infixUFDOps
import Quotient.Element

class RationalFunction[R <: PolynomialOverUFD.Element[R, C, N], C, @specialized(Int, Long) N](override val ring: PolynomialOverField[R, C, N]) extends Quotient[R]()(ring) {
  implicit val rr = ring.ring
  def generators = ring.generators.map(apply)
  def generatorsBy(n: Int) = ring.generatorsBy(n).map(_.map(apply))
  override def random(numbits: Int)(implicit rnd: java.util.Random) = zero
  override def toCode(x: Element[R], precedence: Int) = {
    val Element(n, d) = x
    if (ring.degree(n) == 0 && ring.degree(d) == 0) {
      val nn = ring.headCoefficient(n)
      val dd = ring.headCoefficient(d)
      (nn / dd).toCode(precedence)
    } else super.toCode(x, precedence)
  }
  override def toString = rr.toString + "(" + variables.mkString(", ") + ")"
  override def toMathML(x: Element[R]) = {
    val Element(n, d) = x
    if (ring.degree(n) == 0 && ring.degree(d) == 0) {
      val nn = ring.headCoefficient(n)
      val dd = ring.headCoefficient(d)
      (nn / dd).toMathML
    } else super.toMathML(x)
  }
  override def toMathML = <mrow>{rr.toMathML}<mfenced>{variables.map(_.toMathML)}</mfenced></mrow>

  def variables = ring.variables
}

object RationalFunction {
  def apply[R <: PolynomialOverUFD.Element[R, C, N], C, @specialized(Int, Long) N](ring: PolynomialOverField[R, C, N]) = new RationalFunction(ring)
}
