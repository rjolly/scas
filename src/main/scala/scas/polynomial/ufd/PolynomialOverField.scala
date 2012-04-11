package scas.polynomial.ufd

import scas.structure.Field
import scas.Implicits.infixUFDOps
import PolynomialOverUFD.Element

trait PolynomialOverField[T <: Element[T, C, N], C, N] extends PolynomialOverUFD[T, C, N] {
  override implicit val ring: Field[C]
  def gcd1(x: T, y: T): T = if (y.isZero) x else gcd1(y, monic(reduce(x, y)))
  override def content(x: T) = if (x.isZero) ring.zero else headCoefficient(x)
  override def divide(w: T, y: C) = multiply(w, ring.inverse(y))
  def monic(x: T) = primitivePart(x)
}
