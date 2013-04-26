package scas.polynomial

import scas.ufd.GBGCD
import scas.gb.PairList
import scas.Implicits.{infixOrderingOps, infixPowerProductOps}
import PolynomialWithGB.Element

trait PolynomialWithGB[T <: Element[T, C, N], C, N] extends PolynomialOverUFD[T, C, N] with GBGCD[T, C, N] {
  def gb(xs: T*) = {
    val s = pairs
    s.update(xs)
    s.process
    s.reduce
    s.toSeq
  }
  def pairs = new PairList(this)
  def normalize(x: T) = primitivePart(x)
  def s_polynomial(x: T, y: T) = {
    val (m, a) = head(x)
    val (n, b) = head(y)
    val gcd = pp.gcd(m, n)
    val (m0, n0) = (m / gcd, n / gcd)
    reduce(x * n0, m0, a, y, b)
  }
}

object PolynomialWithGB {
  trait Element[T <: Element[T, C, N], C, N] extends PolynomialOverUFD.Element[T, C, N] { this: T =>
    val factory: PolynomialWithGB[T, C, N]
  }
}
