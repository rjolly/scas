package scas.polynomial.ufd

import scala.reflect.ClassTag
import scas.power.splittable.impl.PowerProduct
import scas.structure.commutative.impl.UniqueFactorizationDomain

trait MultivariatePolynomial[T[C, M], C, M](using ClassTag[T[C, M]])(using ring: UniqueFactorizationDomain[C], pp: PowerProduct[M]) extends PolynomialOverUFD[T[C, M], C, M] {
  val take = pp.take(1)
  val drop = pp.drop(1)
  def split: MultivariatePolynomial[T, T[C, M], M]
  override def gcd(x: T[C, M], y: T[C, M]) = if (variables.length > 1) {
    val s = split
    s.gcd(x.convertTo(using s), y.convertTo(using s)).convertFrom(s)
  } else {
    val (a, p) = contentAndPrimitivePart(x)
    val (b, q) = contentAndPrimitivePart(y)
    primitivePart(gcd1(p, q)).coefMultiply(ring.gcd(a, b))
  }
  extension (x: T[C, M]) def convertTo(using s: MultivariatePolynomial[T, T[C, M], M]): T[T[C, M], M] = iterator(x).foldLeft(s.zero) { (l, r) =>
    val (m, c) = r
    val t = m.projection(0)
    l + s(take.convert(t)(pp), this(drop.convert(m / t)(pp), c))
  }
  extension (x: T[T[C, M], M]) def convertFrom(s: MultivariatePolynomial[T, T[C, M], M]): T[C, M] = s.iterator(x).foldLeft(zero) { (l, r) =>
    val (m, c) = r
    l + c.convert(drop)%* m.convert(take)
  }
}
