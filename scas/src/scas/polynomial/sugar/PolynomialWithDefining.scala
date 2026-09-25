package scas.polynomial.sugar

import scas.polynomial.PolynomialWithSugar
import PolynomialWithDefining.Element

class PolynomialWithDefining[T](using factory: scas.polynomial.binary.PolynomialWithDefining[T]) extends PolynomialWithSugar[scas.polynomial.binary.PolynomialWithDefining.Element[T], Int, Array[Int]] with scas.polynomial.PolynomialWithDefining[Element[T], Int, Array[Int]] {
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

object PolynomialWithDefining {
  type Element[T] = PolynomialWithSugar.Element[scas.polynomial.binary.PolynomialWithDefining.Element[T]]
}
