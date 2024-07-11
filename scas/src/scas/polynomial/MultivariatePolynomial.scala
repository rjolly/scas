package scas.polynomial

import scas.power.PowerProduct
import scas.structure.commutative.UniqueFactorizationDomain

trait MultivariatePolynomial[T[C, M], C, M] extends PolynomialOverUFD[T[C, M], C, M] {
  val take = pp.take(1)
  val drop = pp.drop(1)
  override def newInstance(pp: PowerProduct[M]): MultivariatePolynomial[T, C, M] = newInstance(ring, pp)
  def newInstance: [C] => (UniqueFactorizationDomain[C], PowerProduct[M]) => MultivariatePolynomial[T, C, M]
  def gcd1(x: T[C, M], y: T[C, M]): T[C, M]
  def gcd(x: T[C, M], y: T[C, M]) = if (length > 1) {
    val s = newInstance(newInstance(ring, drop), take)
    s.gcd(x.convertTo(using s), y.convertTo(using s)).convertFrom(s)
  } else {
    val (a, p) = contentAndPrimitivePart(x)
    val (b, q) = contentAndPrimitivePart(y)
    primitivePart(gcd1(p, q))%* ring.gcd(a, b)
  }
  extension (x: T[C, M]) def convertTo(using s: MultivariatePolynomial[T, T[C, M], M]): T[T[C, M], M] = iterator(x).foldLeft(s.zero) { (l, r) =>
    val (m, c) = r
    val t = m.projection(0)
    l + s(take.convert(t)(pp), this(drop.convert(m / t)(pp), c))
  }
  extension (x: T[T[C, M], M]) def convertFrom(s: MultivariatePolynomial[T, T[C, M], M]): T[C, M] = s.iterator(x).foldLeft(zero) { (l, r) =>
    val (m, c) = r
    l + c.convert(drop).ppMultiplyRight(m.convert(take))
  }
}
