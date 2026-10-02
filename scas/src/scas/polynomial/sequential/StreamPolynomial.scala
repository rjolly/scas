package scas.polynomial.sequential

import scas.util.Stream

trait StreamPolynomial[C, M] extends scas.polynomial.StreamPolynomial[C, M] {
  override def apply(s: (M, C)*) = Stream.sequential(s*)
}
