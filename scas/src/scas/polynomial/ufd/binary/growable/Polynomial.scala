package scas.polynomial.ufd.binary.growable

trait Polynomial[T, C] extends scas.polynomial.ufd.binary.Polynomial[T, C] with scas.polynomial.ufd.growable.PolynomialWithGB[T, C, Int] with scas.polynomial.binary.growable.Polynomial[T, C]
