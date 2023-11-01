package scas.polynomial.impl

import scala.annotation.tailrec
import scas.structure.commutative.impl.{Field, EuclidianDomain}
import scas.power.PowerProduct

trait UnivariatePolynomial[T, C, M](using ring: Field[C], val pp: PowerProduct[M]) extends PolynomialOverField[T, C, M] with EuclidianDomain[T] {
  assert (variables.length == 1)
  def derivative(x: T) = x.map((a, b) => (a / pp.generator(0), b * ring.fromInt(pp.degree(a))))
  override def gcd(x: T, y: T) = gcd1(x, y)
  @tailrec final def gcd1(x: T, y: T): T = if (y.isZero) x else gcd1(y, x.reduce(y))
}
