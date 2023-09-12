package scas.polynomial.ufd

import scala.reflect.ClassTag
import scala.annotation.tailrec
import scas.structure.commutative.impl.Field
import scas.power.impl.PowerProduct

trait PolynomialOverField[T : ClassTag, C : Field, M : PowerProduct] extends PolynomialOverUFD[T, C, M] {
  extension (x: T) override def coefDivide(c: C) = x.coefMultiply(Field[C].inverse(c))
  def monic(x: T) = if (x.isZero) zero else x.coefDivide(headCoefficient(x))
  @tailrec final def gcd1(x: T, y: T): T = if (y.isZero) monic(x) else gcd1(y, monic(x.reduce(y)))
  extension (x: T) override def reduce(y: T) = super.reduce(x)(y)
  extension (x: T) override def reduce(m: M, a: C, y: T, b: C) = x.subtract(m, a / b, y)
}
