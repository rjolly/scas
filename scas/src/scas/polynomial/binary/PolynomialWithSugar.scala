package scas.polynomial.binary

import scala.compiletime.deferred
import scas.polynomial.PolynomialWithDefining
import scas.polynomial.PolynomialWithSugar.Element
import PolynomialWithSugar.Impl

class PolynomialWithSugar[T, C](using Polynomial[T, C]) extends Impl[T, C]

object PolynomialWithSugar {
  trait Impl[T, C] extends scas.polynomial.PolynomialWithSugar[T, C, Array[Int]] with PolynomialWithDefining[Element[T], C, Array[Int]] {
    given factory: Polynomial[T, C] = deferred
    def apply(d: Int) = this(factory(d))
    extension (x: Element[T]) {
      def index = x.underlying.index
      def defining = x.underlying.defining
      override def headPowerProduct = x.underlying.headPowerProduct
    }
  }
}
