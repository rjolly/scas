package scas.polynomial.binary.sugar

import Polynomial.Element

class MutablePolynomial[T, C](using factory: scas.polynomial.binary.MutablePolynomial[T, C]) extends Polynomial[T, C] with scas.polynomial.MutablePolynomialWithSugar[Element[T], C, Array[Int]]
