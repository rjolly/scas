package scas.gb

import scas.polynomial.{PolynomialOverUFD, PolynomialWithGB}
import PolynomialOverUFD.Element

trait GB[T <: Element[T, C, N], C, N] extends Engine[T, C, N] { this: PolynomialWithGB[T, C, N] =>
  type P = Pair

  def apply(i: Int, j: Int) = new Pair(i, j)
}
