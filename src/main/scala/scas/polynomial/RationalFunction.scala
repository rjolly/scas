package scas.polynomial

import scas.Variable
import scas.structure.{Quotient, Field}
import scas.Implicits.infixUFDOps
import RationalFunction.Element

class RationalFunction[R <: PolynomialOverUFD.Element[R, C, N], C, @specialized(Int, Long) N](implicit val ring: PolynomialOverField[R, C, N]) extends Quotient[Element[R, C, N], R] {
  def apply(n: R, d: R) = {
    val c = ring(ring.lastCoefficient(d))
    new Element[R, C, N](n / c, d / c)(this)
  }
  def apply(value: C): Element[R, C, N] = apply(ring(value))
  def generators = ring.generators.map(apply)
  def generatorsBy(n: Int) = ring.generatorsBy(n).map(_.map(apply))
  override def random(numbits: Int)(implicit rnd: java.util.Random) = zero
  override def toString = ring.ring.toString + "(" + variables.mkString(", ") + ")"
  override def toMathML = <mrow>{ring.ring.toMathML}<mfenced>{variables.map(_.toMathML)}</mfenced></mrow>

  def variables = ring.variables
}

object RationalFunction {
  def apply[C](ring: Field[C], s: Variable): RationalFunction[tree.UnivariatePolynomial.Element[C, Int], C, Int] = apply(tree.UnivariatePolynomial(ring, s))
  def apply[R <: PolynomialOverUFD.Element[R, C, N], C, @specialized(Int, Long) N](implicit ring: PolynomialOverField[R, C, N]) = new RationalFunction

  class Element[R <: PolynomialOverUFD.Element[R, C, N], C, @specialized(Int, Long) N](val _1: R, val _2: R)(val factory: RationalFunction[R, C, N]) extends Quotient.Element[Element[R, C, N], R] {
    def canEqual(that: Any) = true
  }
  object Element extends ExtraImplicits

  trait ExtraImplicits {
    implicit def coef2rationalFunction[D, R <: PolynomialOverUFD.Element[R, C, N], C, @specialized(Int, Long) N](value: D)(implicit f: D => C, factory: RationalFunction[R, C, N]) = factory(value)
  }
}
