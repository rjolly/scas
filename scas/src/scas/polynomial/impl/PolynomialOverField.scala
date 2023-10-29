package scas.polynomial.impl

import scala.reflect.ClassTag
import scas.structure.commutative.impl.Field
import scas.power.impl.PowerProduct

trait PolynomialOverField[T : ClassTag, C : Field, M : PowerProduct] extends PolynomialOverUFD[T, C, M] {
  given instance: PolynomialOverField[T, C, M]
  extension (x: T) override def %/ (c: C) = x%* Field[C].inverse(c)
  def monic(x: T) = if (x.isZero) zero else x%/ headCoefficient(x)
  extension (x: T) {
    override def reduce(m: M, a: C, y: T, b: C) = x.subtract(m, a / b, y)
    override def reduce(y: T) = super.reduce(x)(y)
  }
  extension (x: T) def modInverse(mod: T) = {
    val s = new PolynomialWithRepr(using this)(1)
    val (p, e) = s.gcd(s(x, 0), s(mod))
    assert (p.isOne)
    e(0)
  }
  extension (ring: Field[C]) def apply(s: T*) = {
    assert (s == generators)
    this
  }
}
