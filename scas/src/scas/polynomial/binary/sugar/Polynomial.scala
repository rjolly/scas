package scas.polynomial.binary.sugar

import scas.polynomial.{PolynomialWithSugar, PolynomialWithDefining}
import Polynomial.Element

class Polynomial[T, C](using factory: scas.polynomial.binary.Polynomial[T, C]) extends PolynomialWithSugar[scas.polynomial.binary.Polynomial.Element[T], C, Array[Int]] with PolynomialWithDefining[Element[T], C, Array[Int]] {
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

object Polynomial {
  type Element[T] = PolynomialWithSugar.Element[scas.polynomial.binary.Polynomial.Element[T]]
}
