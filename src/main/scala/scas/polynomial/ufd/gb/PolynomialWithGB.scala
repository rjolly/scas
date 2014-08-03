package scas.polynomial.ufd.gb

import scas.polynomial.ufd.PolynomialOverUFD
import PolynomialOverUFD.Element

trait PolynomialWithGB[T <: Element[T, C, N], C, N] extends PolynomialWithEngine[T, C, N] {
  type P = Pair

  def apply(i: Int, j: Int) = new Pair(i, j)
}
