package scas.polynomial.ufd

import scala.compiletime.deferred
import scas.structure.commutative.Field

trait PolynomialOverField[T, C, M] extends scas.polynomial.PolynomialOverField[T, C, M] with PolynomialOverUFD[T, C, M] {
  given ring: Field[C] = deferred
  extension (x: T) override def divideRight(c: C) = super[PolynomialOverField].divideRight(x)(c)
  extension (ring: Field[C]) def apply(s: T*) = {
    same(s*)
    this
  }
}
