package scas.polynomial

import scas.structure.{Field, Algebra}

trait PolynomialOverField[T, C, M] extends Polynomial[T, C, M] with Algebra[T, C] {
  given ring: Field[C]
  override def normalize(x: T) = monic(x)
  def monic(x: T) = if (x.isZero) zero else x%/ x.headCoefficient
  extension (x: T) override def reduce(m: M, a: C, y: T, b: C, remainder: Boolean) = x.subtract(m, a / b, y)
  extension (ring: Field[C]) def apply(s: T*) = {
    same(s*)
    this
  }
}
