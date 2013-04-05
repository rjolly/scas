package scas.polynomial

import scala.math.Ordering
import PolynomialWithIndex.Element

trait PolynomialWithIndex[T <: Element[T, C, N], C, N] extends Polynomial[T, C, N] {
  override def compare(x: T, y: T) = {
    val c = Ordering.Int.compare(x.index, y.index)
    if (c < 0) -1
    else if (c > 0) 1
    else super.compare(x, y)
  }
  def apply(x: T, n: Int): T
}

object PolynomialWithIndex {
  trait Element[T <: Element[T, C, N], C, N] extends Polynomial.Element[T, C, N] { this: T =>
    val factory: PolynomialWithIndex[T, C, N]
    val index: Int
  }
}
