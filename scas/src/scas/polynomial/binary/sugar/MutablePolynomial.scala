package scas.polynomial.binary.sugar

class MutablePolynomial[T, C](using scas.polynomial.binary.MutablePolynomial[T, C]) extends Polynomial[T, C] with scas.polynomial.MutablePolynomialWithSugar[scas.polynomial.binary.Polynomial.Element[T], C, Array[Int]]
