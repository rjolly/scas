package scas.polynomial

import scas.util.Stream

trait SequentialStreamPolynomial[C, M] extends StreamPolynomial[C, M] {
  override def apply(s: (M, C)*) = Stream.sequential(s*)
}
