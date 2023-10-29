package scas.polynomial.impl

import scas.power.splittable.impl.PowerProduct
import scas.structure.commutative.impl.UniqueFactorizationDomain

trait MultivariatePolynomial[T[C, M], C, M](using ring: UniqueFactorizationDomain[C], pp: PowerProduct[M]) extends PolynomialOverUFD[T[C, M], C, M] {
  val location = variables.length - 1
  val take = pp.take(location)
  val drop = pp.drop(location)
  def newInstance: [C] => (UniqueFactorizationDomain[C], PowerProduct[M]) => MultivariatePolynomial[T, C, M]
  def gcd1(x: T[C, M], y: T[C, M]): T[C, M]
  override def gcd(x: T[C, M], y: T[C, M]) = if (location > 0) {
    val s = newInstance(newInstance(ring, take), drop)
    s.gcd(x.convertTo(using s), y.convertTo(using s)).convertFrom(s)
  } else {
    val (a, p) = contentAndPrimitivePart(x)
    val (b, q) = contentAndPrimitivePart(y)
    primitivePart(gcd1(p, q))%* ring.gcd(a, b)
  }
  extension (x: T[C, M]) def convertTo(using s: MultivariatePolynomial[T, T[C, M], M]): T[T[C, M], M] = iterator(x).foldLeft(s.zero) { (l, r) =>
    val (m, c) = r
    val t = m.projection(location)
    l + s(drop.convert(t)(pp), this(take.convert(m / t)(pp), c))
  }
  extension (x: T[T[C, M], M]) def convertFrom(s: MultivariatePolynomial[T, T[C, M], M]): T[C, M] = s.iterator(x).foldLeft(zero) { (l, r) =>
    val (m, c) = r
    l + c.convert(take)%* m.convert(drop)
  }
}
