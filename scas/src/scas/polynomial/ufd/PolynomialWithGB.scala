package scas.polynomial.ufd

import scala.reflect.ClassTag
import scas.power.{POT, ModifiedPOT}
import scas.polynomial.ConvertablePolynomial
import scas.module.Array
import scas.base.BigInteger
import BigInteger.given

trait PolynomialWithGB[T[C, M], C, M](using ClassTag[T[C, M]]) extends PolynomialOverUFD[T[C, M], C, M] with scas.polynomial.PolynomialWithGB[T[C, M], C, M] {
  def embedding(name: String, dimension: Int) = newInstance(new ModifiedPOT(pp)(name, dimension))
  def newInstance(pp: POT[M]): PolynomialWithGB[T, C, (M, Array[Int])]
  def gcd(x: T[C, M], y: T[C, M]) = if x.isZero then y else {
    val (a, p) = contentAndPrimitivePart(x)
    val (b, q) = contentAndPrimitivePart(y)
    given module: Module[T, C, M] = new Module(using this)("c", 3)
    val list = module.gb(
      Array(p, 1, 0),
      Array(q, 0, 1)
    )
    val Array(_, u, v) = list.last
    (p / v)%* ring.gcd(a, b)
  }
}
