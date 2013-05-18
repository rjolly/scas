package scas.gb

import scas.polynomial.PolynomialWithSugar
import scala.math.Ordering.Implicits.infixOrderingOps
import PolynomialWithSugar.Element

trait Fussy[T <: Element[T, C, N], C, N] extends Sugar[T, C, N] { this: PolynomialWithSugar[T, C, N] =>
  type P = Pair

  override implicit def natural = ordering

  override def factorOf(p1: P, p2: P) = super.factorOf(p1, p2) && (p1 < p2)

  def apply(i: Int, j: Int) = new Pair(i, j)
}
