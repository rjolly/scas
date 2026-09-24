package scas.polynomial.sugar

import scas.polynomial.PolynomialWithSugar
import PolynomialWithDefining.Element

class PolynomialWithDefining[T](using factory: scas.polynomial.PolynomialWithDefining[T]) extends PolynomialWithSugar[scas.polynomial.PolynomialWithDefining.Element[T], Int, Array[Int]] {
  override def s_polynomial(x: Element[T], y: Element[T]) = {
    val (p, _) = x
    val (q, _) = y
    p match {
      case Right(_) => q match {
        case Right(_) => super.s_polynomial(x, y)
        case Left(d) => x * generator(d)
      }
      case Left(d) => q match {
        case Right(_) => generator(d) * y
        case Left(_) => ???
      }
    }
  }
  extension (x: Element[T]) {
    override def headPowerProduct = {
      val (p, _) = x
      p.headPowerProduct
    }
    override def reduce(ys: Element[T]*) = {
      val (p, _) = x
      if (p.isRight) then super.reduce(x)(ys.filter(y => {
        val (q, _) = y
        q.isRight
      })*)
      else x
    }
    override def reduce(strict: Boolean, tail: Boolean, ys: Element[T]*) = {
      val (p, _) = x
      if (p.isRight) then super.reduce(x)(strict, tail, ys.filter(y => {
        val (q, _) = y
        q.isRight
      })*)
      else x
    }
  }
}

object PolynomialWithDefining {
  type Element[T] = PolynomialWithSugar.Element[scas.polynomial.PolynomialWithDefining.Element[T]]
}
