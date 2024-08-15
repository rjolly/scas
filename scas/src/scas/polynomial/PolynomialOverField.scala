package scas.polynomial

import scala.reflect.ClassTag
import scas.structure.commutative.Field
import scas.module.ArrayModule

trait PolynomialOverField[T : ClassTag, C, M] extends PolynomialOverUFD[T, C, M] {
  given ring: Field[C]
  extension (x: T) override def %/ (c: C) = x%* ring.inverse(c)
  def monic(x: T) = if (x.isZero) zero else x%/ headCoefficient(x)
  extension (x: T) def modInverse(mods: T*): T
  extension (ring: Field[C]) def apply(s: T*) = {
    given ArrayModule[T] = ArrayModule(this)(variables.length)
    assert (s.toArray >< generators.toArray)
    this
  }
}
