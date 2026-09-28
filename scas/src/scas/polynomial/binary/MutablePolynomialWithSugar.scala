package scas.polynomial.binary

import scala.compiletime.deferred
import MutablePolynomialWithSugar.Impl

class MutablePolynomialWithSugar[T, C](using MutablePolynomial[T, C]) extends Impl[T, C]

object MutablePolynomialWithSugar {
  trait Impl[T, C] extends PolynomialWithSugar.Impl[T, C] with scas.polynomial.MutablePolynomialWithSugar[T, C, Array[Int]] {
    given factory: MutablePolynomial[T, C] = deferred
  }
}
