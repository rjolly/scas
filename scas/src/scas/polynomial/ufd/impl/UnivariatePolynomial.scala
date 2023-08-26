package scas.polynomial.ufd.impl

import scala.reflect.ClassTag
import scas.structure.commutative.impl.{Field, EuclidianDomain}
import scas.power.impl.PowerProduct

trait UnivariatePolynomial[T : ClassTag, C, M](using ring: Field[C], pp: PowerProduct[M]) extends PolynomialOverField[T, C, M] with EuclidianDomain[T] {
  assert (variables.length == 1)
  def derivative(x: T) = x.map((a, b) => (a.divide(pp.generator(0)), b.multiply(ring(pp.degree(a)))))
  override def gcd(x: T, y: T) = gcd1(x, y)
  extension (x: T) def modInverse(mod: T): T
}
