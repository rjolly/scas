package scas.gb

import scas.polynomial.PolynomialWithSugar
import PolynomialWithSugar.Element

trait Sloppy[T <: Element[T, C, N], C, N] extends Sugar[T, C, N] { this: PolynomialWithSugar[T, C, N] =>
  type P = Pair

  def apply(i: Int, j: Int) = new Pair(i, j)
}
