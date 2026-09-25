package scas.polynomial

import scas.base.BigInteger.{max, given}
import scas.polynomial.PolynomialWithSugar
import PolynomialWithSugar.Element

class MutablePolynomialWithSugar[T, C, M](using factory: MutablePolynomial[T, C, M]) extends PolynomialWithSugar[T, C, M] {
  def unmodifiable(x: Element[T]) = {
    val (p, e) = x
    (factory.unmodifiable(p), e)
  }
  def modifiable(x: Element[T]) = {
    val (p, e) = x
    (factory.modifiable(p), e)
  }
  extension (x: Element[T]) {
    override def subtract(m: M, c: C, y: Element[T]) = {
      val (p, e) = x
      val (q, f) = y
      (p.subtract(m, c, q), max(e, f + m.degree))
    }
    override def multiplyRight(c: C) = {
      val (p, e) = x
      (p%* c, e)
    }
  }
}
