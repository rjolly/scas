package scas.polynomial

import scala.annotation.tailrec
import scas.structure.Field
import scas.Implicits.{infixUFDOps, infixPowerProductOps}
import PolynomialOverUFD.Element

trait PolynomialOverField[T <: Element[T, C, N], C, N] extends PolynomialOverUFD[T, C, N] {
  implicit val ring: Field[C]
  override def divide(x: T, c: C) = multiply(x, ring.inverse(c))
  def monic(x: T) = if (x.isZero) zero else divide(x, headCoefficient(x))
  @tailrec final def gcd1(x: T, y: T): T = if (y.isZero) monic(x) else gcd1(y, monic(reduce(x, y)))
  override def reduce(x: T, m: Array[N], a: C, y: T, b: C) = subtract(x, m, a / b, y)
}
