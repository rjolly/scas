package scas.polynomial.binary

import scala.compiletime.deferred
import scas.polynomial.PolynomialWithDefining
import PolynomialWithSugar.Element

trait PolynomialWithSugar[T, C] extends scas.polynomial.PolynomialWithSugar[Polynomial.Element[T], C, Array[Int]] with PolynomialWithDefining[Element[T], C, Array[Int]] {
  given factory: Polynomial[T, C] = deferred
  extension (x: Element[T]) {
    def index = x.underlying.index
    def defining = x.underlying.defining
    override def headPowerProduct = x.underlying.headPowerProduct
  }
}

object PolynomialWithSugar {
  type Element[T] = scas.polynomial.PolynomialWithSugar.Element[Polynomial.Element[T]]
}
