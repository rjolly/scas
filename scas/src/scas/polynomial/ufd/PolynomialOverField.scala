package scas.polynomial.ufd

import scala.reflect.ClassTag
import scala.annotation.{tailrec, targetName}
import scas.structure.commutative.Field
import scas.power.PowerProduct

trait PolynomialOverField[T : ClassTag, C : Field, M : PowerProduct] extends PolynomialOverUFD[T, C, M] {
  extension (x: T) @targetName("coefDivide") override def divide(c: C) = x.multiply(Field[C].inverse(c))
  def monic(x: T) = if (x.isZero) zero else x.divide(headCoefficient(x))
  @tailrec final def gcd1(x: T, y: T): T = if (y.isZero) monic(x) else gcd1(y, monic(x.reduce(y)))
  extension (x: T) override def reduce(y: T) = super.reduce(x)(y)
  extension (x: T) override def reduce(m: M, a: C, y: T, b: C) = x.subtract(m, a / b, y)
}
