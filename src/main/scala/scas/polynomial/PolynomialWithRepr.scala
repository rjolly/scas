package scas.polynomial

import scas.Implicits.infixRingOps
import PolynomialWithRepr.Element

trait PolynomialWithRepr[S[C, N] <: Polynomial.Element[S[C, N], C, N], T <: Element[S, T, C, N], C, N] extends Polynomial[T, C, N] {
  val module: Module[S[C, N], C, N]
  abstract override def convert(x: T) = apply(super.convert(x), module.convert(x.element))
  abstract override def plus(x: T, y: T) = apply(super.plus(x, y), x.element + y.element)
  override def times(x: T, m: Array[N]) = apply(super.times(x, m), module.multiply(x.element, m, ring.one))
  override def subtract(x: T, m: Array[N], c: C, y: T) = apply(super.subtract(x, m, c, y), x.element - module.multiply(y.element, m, c))
  override def multiply(x: T, m: Array[N], c: C) = apply(super.multiply(x, m, c), module.multiply(x.element, m, c))
  override def multiply(x: T, c: C) = apply(super.multiply(x, c), module.multiply(x.element, c))
  def apply(x: T, n: Int): T = apply(x, module.generator(n))
  def apply(x: T, element: Module.Element[S[C, N]]): T
  def fromPolynomial(x: S[C, N]): T
}

object PolynomialWithRepr {
  trait Element[S[C, N] <: Polynomial.Element[S[C, N], C, N], T <: Element[S, T, C, N], C, N] extends Polynomial.Element[T, C, N] { this: T =>
    val factory: PolynomialWithRepr[S, T, C, N]
    val element: Module.Element[S[C, N]]
  }
}
