package scas.gb

import scala.math.Ordering.Implicits.infixOrderingOps
import PolynomialWithSugar.Element

trait PolynomialWithFussy[T <: Element[T, C, N], C, N] extends PolynomialWithSugar[T, C, N] {
  override implicit def natural = ordering

  override def factorOf(p1: P, p2: P) = super.factorOf(p1, p2) && (p1 < p2)
}
