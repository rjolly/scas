package scas.polynomial

import scas.structure.Field
import scas.Implicits.{infixUFDOps, infixPowerProductOps}
import PolynomialOverUFD.Element

trait PolynomialOverField[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends PolynomialOverUFD[T, C, N] {
  override implicit val ring: Field[C]
  override def divide(w: T, y: C) = multiply(w, ring.inverse(y))
  override def content(x: T) = if (x.isZero) ring.zero else headCoefficient(x)
  def monic(x: T) = primitivePart(x)
  override def subtract(x: T, m: Array[N], a: C, y: T, b: C) = x - multiply(y, m, a / b)
}
