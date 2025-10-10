package scas.polynomial.ufd

import scala.compiletime.deferred
import scas.power.splitable.PowerProduct
import scas.structure.commutative.UniqueFactorizationDomain

trait MultivariatePolynomial[T[C, M], C, M] extends PolynomialOverUFD[T[C, M], C, M] {
  given pp: PowerProduct[M] = deferred
  val take = pp.take(1)
  val drop = pp.drop(1)
  def newInstance: [C] => (UniqueFactorizationDomain[C], PowerProduct[M]) => MultivariatePolynomial[T, C, M]
  def gcd1(x: T[C, M], y: T[C, M]): T[C, M]
  def gcd(x: T[C, M], y: T[C, M]) = if pp.length > 1 then {
    val p = newInstance(ring, drop)
    val s = newInstance(p, take)
    s.gcd(x.convertTo(using p, s), y.convertTo(using p, s)).convertFrom(s)
  } else {
    val (a, p) = contentAndPrimitivePart(x)
    val (b, q) = contentAndPrimitivePart(y)
    primitivePart(gcd1(p, q))%* ring.gcd(a, b)
  }
  extension (x: T[C, M]) def convert(from: PowerProduct[M]) = x.map((s, a) => (s.convert(from), a)).sort
  extension (x: T[C, M]) def convertTo(using p: MultivariatePolynomial[T, C, M], s: MultivariatePolynomial[T, T[C, M], M]): T[T[C, M], M] = x.iterator.foldLeft(s.zero) { (l, r) =>
    val (m, c) = r
    val t = m.projection(0)
    l + s(take.convert(t)(pp), p(drop.convert(m / t)(pp), c))
  }
  extension (x: T[T[C, M], M]) def convertFrom(s: MultivariatePolynomial[T, T[C, M], M]): T[C, M] = s.iterator(x).foldLeft(zero) { (l, r) =>
    val (m, c) = r
    l + c.convert(drop)%* m.convert(take)
  }
}
