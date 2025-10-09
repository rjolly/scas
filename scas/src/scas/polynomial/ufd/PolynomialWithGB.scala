package scas.polynomial.ufd

import scala.compiletime.deferred
import scala.reflect.ClassTag
import scas.math.Numeric
import scas.power.{ArrayPowerProduct, POT, ModifiedPOT}
import scas.polynomial.gb.GBEngine
import scas.module.Array
import scas.base.BigInteger
import BigInteger.given

trait PolynomialWithGB[T : ClassTag, C, N : {Numeric, ClassTag}] extends PolynomialOverUFD[T, C, Array[N]] {
  given pp: ArrayPowerProduct[N] = deferred
  extension (x: T) def convert(from: ArrayPowerProduct[N]) = x.map((s, a) => (s.convert(from), a)).sort
  def embedding(name: String, dimension: Int) = newInstance(new ModifiedPOT(pp, name, dimension))
  def newInstance(pp: POT[N]): PolynomialWithGB[T, C, N]
  def gcd(x: T, y: T) = {
    val (a, p) = contentAndPrimitivePart(x)
    val (b, q) = contentAndPrimitivePart(y)
    given module: Module[T, C, N] = new Module(using this)("c", 3)
    val list = module.gb(
      Array(p, 1, 0),
      Array(q, 0, 1)
    )
    val Array(_, u, v) = list.last
    (p / v)%* ring.gcd(a, b)
  }
  def gb(xs: T*) = new GBEngine(using this).gb(xs*)
}
