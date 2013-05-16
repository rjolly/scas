package scas.gb

import scas.polynomial.PolynomialWithSugar
import PolynomialWithSugar.Element

class Sloppy[T <: Element[T, C, N], C, N](val ring: PolynomialWithSugar[T, C, N]) extends Sugar[T, C, N] {
  type P = Pair

  def apply(i: Int, j: Int) = new Pair(i, j)
}
