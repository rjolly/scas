package scas.polynomial.ufd

import scala.reflect.ClassTag
import scala.annotation.tailrec
import scas.structure.commutative.EuclidianDomain

trait UnivariatePolynomial[T : ClassTag, C, M] extends PolynomialWithModInverse[T, C, M] with EuclidianDomain[T] {
  assert (pp.length == 1)
  def derivative(x: T) = x.map((a, b) => (a / pp.generator(0), b * ring.fromInt(a.degree)))
  override def gcd(x: T, y: T) = gcd1(x, y)
  @tailrec final def gcd1(x: T, y: T): T = if y.isZero then x else gcd1(y, x.reduce(y))
  extension (x: T) {
    override def reduce(m: M, a: C, y: T, b: C, remainder: Boolean) = x.subtract(m, a / b, y)
    override def reduce(ys: T*) = super.reduce(x)(ys*)
  }
  extension (x: T) def modInverse(mods: T*) = {
    assert (mods.length == 1)
    val s = new scas.polynomial.repr.UnivariatePolynomial(this)(1)
    val (p, e) = s.gcd(s(x, 0), s(mods(0)))
    assert (p.isUnit)
    e(0) / p
  }
}
