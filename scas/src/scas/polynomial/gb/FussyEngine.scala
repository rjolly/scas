package scas.polynomial.gb

import scas.polynomial.Polynomial
import scas.math.Ordering.given

class FussyEngine[T, C, M](factory: Polynomial[T, C, M]) extends SugarEngine(factory) {
  override def natural = ordering

  extension (p1: SugarPair[M]) override def | (p2: SugarPair[M]) = super.|(p1)(p2) && (p1 < p2)
}
