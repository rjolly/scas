package scas.polynomial.tree

import Polynomial.Element

trait SolvablePolynomial[C, @specialized(Int, Long) N] extends Polynomial[C, N] with scas.polynomial.SolvablePolynomial[Element[C, N], C, N]
