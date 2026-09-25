package scas.polynomial.binary

import scala.compiletime.deferred
import scas.polynomial.PolynomialWithDefining
import PolynomialWithSugar.Element

trait PolynomialWithSugar[T, C] extends scas.polynomial.PolynomialWithSugar[Polynomial.Element[T], C, Array[Int]] with PolynomialWithDefining[Element[T], C, Array[Int]] {
  given factory: Polynomial[T, C] = deferred
  extension (x: Element[T]) {
    def index = {
      val (p, _) = x
      p.index
    }
    def defining = {
      val (p, _) = x
      p.defining
    }
    override def headPowerProduct = {
      val (p, _) = x
      p.headPowerProduct
    }
  }
}

object PolynomialWithSugar {
  type Element[T] = scas.polynomial.PolynomialWithSugar.Element[Polynomial.Element[T]]
}
