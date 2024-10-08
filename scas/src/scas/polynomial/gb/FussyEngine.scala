package scas.polynomial.gb

import scas.polynomial.PolynomialWithSugar
import scas.math.Ordering.given

class FussyEngine[T, C, M](using factory: PolynomialWithSugar[T, C, M]) extends SugarEngine[T, C, M] {
  override def natural = ordering

  extension (p1: SugarPair[M]) override def | (p2: SugarPair[M]) = super.|(p1)(p2) && (p1 < p2)
}
