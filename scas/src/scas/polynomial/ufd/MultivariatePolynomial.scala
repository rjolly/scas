package scas.polynomial.ufd

import scala.compiletime.deferred
import scas.polynomial.ConvertablePolynomial
import scas.power.splitable.ArrayPowerProduct
import scas.structure.commutative.UniqueFactorizationDomain

trait MultivariatePolynomial[T[C, M], C, N] extends PolynomialOverUFD[T[C, Array[N]], C, Array[N]] with ConvertablePolynomial[T[C, Array[N]], C, N] {
  given pp: ArrayPowerProduct[N] = deferred
  val take = pp.take(1)
  val drop = pp.drop(1)
  def newInstance: [C] => (UniqueFactorizationDomain[C], ArrayPowerProduct[N]) => MultivariatePolynomial[T, C, N]
  def gcd1(x: T[C, Array[N]], y: T[C, Array[N]]): T[C, Array[N]]
  def gcd(x: T[C, Array[N]], y: T[C, Array[N]]) = if pp.length > 1 then {
    val p = newInstance(ring, drop)
    val s = newInstance(p, take)
    s.gcd(x.convertTo(using p, s), y.convertTo(using p, s)).convertFrom(s)
  } else {
    val (a, p) = contentAndPrimitivePart(x)
    val (b, q) = contentAndPrimitivePart(y)
    primitivePart(gcd1(p, q))%* ring.gcd(a, b)
  }
  extension (x: T[C, Array[N]]) def convertTo(using p: MultivariatePolynomial[T, C, N], s: MultivariatePolynomial[T, T[C, Array[N]], N]): T[T[C, Array[N]], Array[N]] = x.iterator.foldLeft(s.zero) { (l, r) =>
    val (m, c) = r
    val t = m.projection(0)
    l + s(take.convert(t)(pp), p(drop.convert(m / t)(pp), c))
  }
  extension (x: T[T[C, Array[N]], Array[N]]) def convertFrom(s: MultivariatePolynomial[T, T[C, Array[N]], N]): T[C, Array[N]] = s.iterator(x).foldLeft(zero) { (l, r) =>
    val (m, c) = r
    l + c.convert(drop)%* m.convert(take)
  }
}
