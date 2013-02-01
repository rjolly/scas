package scas.polynomial

import scas.ufd.GBGCD
import scas.gb.GBEngine
import PolynomialWithGB.Element

trait PolynomialWithGB[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends PolynomialWithIndex[T, C, N] with PolynomialOverUFD[T, C, N] with GBEngine[T, C, N] with GBGCD[T, C, N] {
  def gb(xs: T*): List[T] = gb(xs.toList)
}

object PolynomialWithGB {
  trait Element[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends PolynomialWithIndex.Element[T, C, N] with PolynomialOverUFD.Element[T, C, N] { this: T =>
    val factory: PolynomialWithGB[T, C, N]
  }
}
