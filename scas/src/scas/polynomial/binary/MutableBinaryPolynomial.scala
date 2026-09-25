package scas.polynomial.binary

import scas.polynomial.MutablePolynomial

trait MutableBinaryPolynomial[T, C] extends BinaryPolynomial[T, C] with MutablePolynomial[T, C, Array[Int]]
