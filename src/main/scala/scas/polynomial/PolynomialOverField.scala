package scas.polynomial

import scas.structure.Field
import scas.Implicits.{infixUFDOps, infixPowerProductOps}
import PolynomialOverUFD.Element

trait PolynomialOverField[T <: Element[T, C, N], C, N] extends PolynomialOverUFD[T, C, N] {
  override implicit val ring: Field[C]
  override def divide(w: T, y: C) = multiply(w, ring.inverse(y))
  def monic(x: T) = if (x.isZero) zero else divide(x, headCoefficient(x))
}
