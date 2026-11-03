package scas.polynomial.mutable

import scala.compiletime.deferred
import scas.polynomial.PolynomialWithSignature.Element

trait PolynomialWithSignature[T, C, M] extends scas.polynomial.PolynomialWithSignature[T, C, M] with Polynomial[Element[T, M], C, M] {
  given factory: Polynomial[T, C, M] = deferred
  import module.{max, pp2pot}
  def unmodifiable(x: Element[T, M]) = {
    val (p, e) = x
    (factory.unmodifiable(p), e)
  }
  def modifiable(x: Element[T, M]) = {
    val (p, e) = x
    (factory.modifiable(p), e)
  }
  extension (x: Element[T, M]) {
    override def subtract(m: M, c: C, y: Element[T, M]) = {
      val (p, e) = x
      val (q, f) = y
      (p.subtract(m, c, q), max(e, f * m))
    }
    override def multiplyRight(c: C) = {
      val (p, e) = x
      (p%* c, e)
    }
  }
}
