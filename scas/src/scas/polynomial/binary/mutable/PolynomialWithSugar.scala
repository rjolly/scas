package scas.polynomial.binary.mutable

import scala.compiletime.deferred
import PolynomialWithSugar.Impl

class PolynomialWithSugar[T, C](using Polynomial[T, C]) extends Impl[T, C]

object PolynomialWithSugar {
  trait Impl[T, C] extends scas.polynomial.binary.PolynomialWithSugar.Impl[T, C] with scas.polynomial.mutable.PolynomialWithSugar[T, C, Array[Int]] {
    given factory: Polynomial[T, C] = deferred
  }
}
