package scas.gb

import scas.polynomial.{PolynomialOverUFD, PolynomialWithGB}
import PolynomialOverUFD.Element

class GB[T <: Element[T, C, N], C, N](val ring: PolynomialWithGB[T, C, N]) extends Engine[T, C, N] {
  type P = Pair

  def apply(i: Int, j: Int) = new Pair(i, j)
}
