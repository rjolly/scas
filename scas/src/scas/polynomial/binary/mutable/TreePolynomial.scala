package scas.polynomial.binary.mutable

import scas.polynomial.binary.TreePolynomial.Element

trait TreePolynomial[C] extends scas.polynomial.binary.TreePolynomial[C] with Polynomial[Element[C], C]
