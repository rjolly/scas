package scas.polynomial.ufd

import scala.compiletime.deferred
import scas.polynomial.ConvertablePolynomial
import scas.power.splitable.ArrayPowerProduct
import scas.structure.commutative.UniqueFactorizationDomain

trait MultivariatePolynomial[T[C], C, N] extends PolynomialOverUFD[T[C], C, Array[N]] with ConvertablePolynomial[T[C], C, N] {
  given pp: ArrayPowerProduct[N] = deferred
  val take = pp.take(1)
  val drop = pp.drop(1)
  lazy val p = newInstance(ring, drop)
  given s: MultivariatePolynomial[T, T[C], N] = newInstance(p, take)
  def newInstance: [C] => (UniqueFactorizationDomain[C], ArrayPowerProduct[N]) => MultivariatePolynomial[T, C, N]
  def gcd1(x: T[C], y: T[C]): T[C]
  def gcd(x: T[C], y: T[C]) = if pp.nbvars > 1 then {
    s.gcd(x.convertTo, y.convertTo).convertFrom
  } else {
    val (a, p) = contentAndPrimitivePart(x)
    val (b, q) = contentAndPrimitivePart(y)
    primitivePart(gcd1(p, q))%* ring.gcd(a, b)
  }
  extension (x: T[C]) def convertTo: T[T[C]] = x.iterator.foldLeft(s.zero) { (l, r) =>
    val (m, c) = r
    val t = m.projection(0)
    l + s(take.convert(t)(pp), p(drop.convert(m / t)(pp), c))
  }
  extension (x: T[T[C]]) def convertFrom: T[C] = x.iterator.foldLeft(zero) { (l, r) =>
    val (m, c) = r
    l + c.convert(drop)%* m.convert(take)
  }
}
