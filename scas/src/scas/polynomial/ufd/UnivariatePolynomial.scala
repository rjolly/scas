package scas.polynomial.ufd

import scala.reflect.ClassTag
import scas.structure.commutative.{Field, EuclidianDomain}
import scas.power.PowerProduct

trait UnivariatePolynomial[T : ClassTag, C : Field.Impl, M : PowerProduct.Impl] extends UnivariatePolynomial.Impl[T, C, M] with PolynomialOverField[T, C, M] with EuclidianDomain[T]

object UnivariatePolynomial {
  trait Impl[T : ClassTag, C, M](using ring: Field.Impl[C], pp: PowerProduct.Impl[M]) extends PolynomialOverField.Impl[T, C, M] with EuclidianDomain.Impl[T] {
    assert (variables.length == 1)
    def derivative(x: T) = x.map((a, b) => (a.divide(pp.generator(0)), b.multiply(ring(pp.degree(a)))))
    override def gcd(x: T, y: T) = gcd1(x, y)
    extension (x: T) def modInverse(mod: T): T
  }
}
