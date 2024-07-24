package scas.polynomial

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.power.ArrayPowerProduct

trait PolynomialWithGB[T : ClassTag, C, N : Numeric : ClassTag] extends PolynomialOverUFD[T, C, Array[N]] {
  given pp: ArrayPowerProduct[N]
  def newInstance(pp: ArrayPowerProduct[N]): PolynomialWithGB[T, C, N]
  def normalize(x: T) = primitivePart(x)
  def s_polynomial(x: T, y: T) = {
    val (m, a) = head(x)
    val (n, b) = head(y)
    val gcd = pp.gcd(m, n)
    val (m0, n0) = (m / gcd, n / gcd)
    x.ppMultiplyRight(n0).reduce(m0, a, y, b)
  }
  def gcd(x: T, y: T) = {
    val (a, p) = contentAndPrimitivePart(x)
    val (b, q) = contentAndPrimitivePart(y)
    given module: Module[T, C, N] = new Module(using this)("c", 3)
    val list = module.gb(
      Array(p, one, zero),
      Array(q, zero, one)
    )
    val Array(_, u, v) = list.last
    (p / v)%* ring.gcd(a, b)
  }
  def gb(xs: T*) = {
    new Engine(using this).process(xs)
  }
}
