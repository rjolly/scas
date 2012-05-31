package scas.polynomial

import PolynomialWithSyzygy.Element

trait PolynomialWithSyzygy[S[C, N] <: Polynomial.Element[S[C, N], C, N], T <: Element[S, T, C, N], C, @specialized(Int, Long) N] extends Polynomial[T, C, N] {
  val module: Module[S[C, N], C, N]
  abstract override def convert(x: T) = apply(super.convert(x), module.convert(x.element))
  abstract override def plus(x: T, y: T) = apply(super.plus(x, y), x.element + y.element)
  abstract override def minus(x: T, y: T) = apply(super.minus(x, y), x.element - y.element)
  override def multiply(w: T, x: Array[N], y: C) = apply(super.multiply(w, x, y), module.multiply(w.element, x, y))
  override def multiply(w: T, y: C) = apply(super.multiply(w, y), module.multiply(w.element, y))
  def apply(x: T, n: Int): T = apply(x, module.generators(n))
  def apply(x: T, element: Module.Element[S[C, N]]): T
  def fromPolynomial(x: S[C, N]): T
}

object PolynomialWithSyzygy {
  trait Element[S[C, N] <: Polynomial.Element[S[C, N], C, N], T <: Element[S, T, C, N], C, @specialized(Int, Long) N] extends Polynomial.Element[T, C, N] { this: T =>
    val factory: PolynomialWithSyzygy[S, T, C, N]
    val element: Module.Element[S[C, N]]
  }
}
