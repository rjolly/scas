package scas.polynomial

import scas.structure.Field
import scas.ufd.MonicGCD
import scas.Implicits.{infixUFDOps, infixPowerProductOps}
import PolynomialOverUFD.Element

trait PolynomialOverField[T <: Element[T, C, N], C, N] extends PolynomialOverUFD[T, C, N] with MonicGCD[T, C, N] {
  implicit val ring: Field[C]
  override def divide(x: T, c: C) = multiply(x, ring.inverse(c))
  def monic(x: T) = if (x.isZero) zero else divide(x, headCoefficient(x))
  override def reduce(x: T, m: Array[N], a: C, y: T, b: C) = subtract(x, m, a/b, y)
}
