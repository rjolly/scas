package scas.polynomial.impl

import scala.reflect.ClassTag
import scas.structure.commutative.impl.{Field, EuclidianDomain}
import scas.power.impl.PowerProduct

trait UnivariatePolynomial[T : ClassTag, C, M](using ring: Field[C], pp: PowerProduct[M]) extends PolynomialOverField[T, C, M] with EuclidianDomain[T] {
  assert (variables.length == 1)
  def derivative(x: T) = x.map((a, b) => (a / pp.generator(0), b * ring.fromInt(pp.degree(a))))
  override def gcd(x: T, y: T) = gcd1(x, y)
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