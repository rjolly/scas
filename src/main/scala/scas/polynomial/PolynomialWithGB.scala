package scas.polynomial

import scas.ufd.GBGCD
import scas.gb.PairList
import scas.Implicits.{infixOrderingOps, infixPowerProductOps}
import PolynomialWithGB.Element

trait PolynomialWithGB[T <: Element[T, C, N], C, N] extends PolynomialWithIndex[T, C, N] with PolynomialOverUFD[T, C, N] with GBGCD[T, C, N] {
  def gb(xs: T*): List[T] = gb(xs.toList)

  def gb(list: List[T]) = pairs(list).process

  def pairs(list: List[T]) = new PairList[T, C, N](list)(this)

  def s_polynomial(x: T, y: T) = {
    val (m, a) = head(x)
    val (n, b) = head(y)
    val gcd = pp.gcd(m, n)
    val (m0, n0) = (m / gcd, n / gcd)
    reduce(x * n0, m0, a, y, b)
  }
}

object PolynomialWithGB {
  trait Element[T <: Element[T, C, N], C, N] extends PolynomialWithIndex.Element[T, C, N] with PolynomialOverUFD.Element[T, C, N] { this: T =>
    val factory: PolynomialWithGB[T, C, N]
  }
}
