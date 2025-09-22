package scas.polynomial

import scala.compiletime.deferred
import scas.structure.{Field, Algebra}

trait PolynomialOverField[T, C, M] extends Polynomial[T, C, M] with Algebra[T, C] {
  given ring: Field[C] = deferred
  override def normalize(x: T) = monic(x)
  def monic(x: T) = if x.isZero then zero else x%/ x.headCoefficient
  extension (x: T) override def reduce(m: M, a: C, y: T, b: C, strict: Boolean) = x.subtract(m, a / b, y)
  extension (ring: Field[C]) def apply(s: T*) = {
    same(s*)
    this
  }
}
