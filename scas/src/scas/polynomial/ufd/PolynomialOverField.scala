package scas.polynomial.ufd

import scala.reflect.ClassTag
import scala.annotation.tailrec
import scas.structure.commutative.Field
import scas.power.PowerProduct

trait PolynomialOverField[T : ClassTag, C : Field.Impl, M : PowerProduct.Impl] extends PolynomialOverField.Impl[T, C, M] with PolynomialOverUFD[T, C, M]

object PolynomialOverField {
  trait Impl[T : ClassTag, C : Field.Impl, M : PowerProduct.Impl] extends PolynomialOverUFD.Impl[T, C, M] {
    extension (x: T) override def coefDivide(c: C) = x.coefMultiply(Field[C].inverse(c))
    def monic(x: T) = if (x.isZero) zero else x.coefDivide(headCoefficient(x))
    @tailrec final def gcd1(x: T, y: T): T = if (y.isZero) monic(x) else gcd1(y, monic(x.reduce(y)))
    extension (x: T) override def reduce(y: T) = super.reduce(x)(y)
    extension (x: T) override def reduce(m: M, a: C, y: T, b: C) = x.subtract(m, a.divide(b), y)
  }
}
