package scas.polynomial

import scas.Variable
import scas.structure.Field
import Residue.Element

trait Residue[R <: PolynomialOverUFD.Element[R, C, N], C, @specialized(Int, Long) N] extends scas.structure.Residue[Element[R, C, N], R] {
  val ring: PolynomialOverUFD[R, C, N]
  var list = List.empty[R]
  def generators = ring.generators.map(apply)
  def apply(value: C): Element[R, C, N] = apply(ring(value))
  def apply(value: R) = new Element[R, C, N](value)(this)
  def reduce(value: R) = apply(ring.remainder(value, list))
  def unapply(x: Element[R, C, N]) = Some(x.value)
  def characteristic = ring.characteristic
  override def toString = ring.ring.toString + "(" + list.mkString(", ") + ")"
  def toMathML = <msub>{ring.ring.toMathML}<mrow>{list.map(_.toMathML)}</mrow></msub>
}

object Residue {
  def apply[C](ring: Field[C], s: Variable): AlgebraicNumber[tree.UnivariatePolynomial.Element[C, Int], C, Int] = apply(tree.UnivariatePolynomial(ring, s))
  def apply[R <: PolynomialOverUFD.Element[R, C, N], C, @specialized(Int, Long) N](ring: UnivariatePolynomial[R, C, N]) = new AlgebraicNumberImpl(ring)

  class Element[R <: PolynomialOverUFD.Element[R, C, N], C, @specialized(Int, Long) N](val value: R)(val factory: Residue[R, C, N]) extends scas.structure.Residue.Element[Element[R, C, N], R]
  object Element extends ExtraImplicits

  trait ExtraImplicits {
    implicit def coef2residue[D, R <: PolynomialOverUFD.Element[R, C, N], C, @specialized(Int, Long) N](value: D)(implicit f: D => C, factory: Residue[R, C, N]) = factory(value)
  }
}
