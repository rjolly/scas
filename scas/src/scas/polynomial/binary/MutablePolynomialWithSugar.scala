package scas.polynomial.binary

import scala.compiletime.deferred

trait MutablePolynomialWithSugar[T, C] extends PolynomialWithSugar[T, C] with scas.polynomial.MutablePolynomialWithSugar[T, C, Array[Int]] {
  given factory: MutablePolynomial[T, C] = deferred
}
