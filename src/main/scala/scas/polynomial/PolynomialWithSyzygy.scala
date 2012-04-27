package scas.polynomial

import scas.module.Module
import PolynomialWithSyzygy.Element

trait PolynomialWithSyzygy[T <: Element[T, C, N], C, N] extends Polynomial[T, C, N] {
  val module: Syzygy[scas.polynomial.tree.Polynomial.Element[C, N], C, N]
  abstract override def apply(x: T) = apply(super.apply(x), module(x.syzygy))
  abstract override def plus(x: T, y: T) = apply(super.plus(x, y), x.syzygy + y.syzygy)
  abstract override def minus(x: T, y: T) = apply(super.minus(x, y), x.syzygy - y.syzygy)
  override def multiply(w: T, x: Array[N], y: C) = apply(super.multiply(w, x, y), module.multiply(w.syzygy, x, y))
  override def multiply(w: T, y: C) = apply(super.multiply(w, y), module.multiply(w.syzygy, y))
  def apply(x: T, n: Int): T = apply(x, module.generators(n))
  def apply(x: T, sysygy: Module.Element[scas.polynomial.tree.Polynomial.Element[C, N]]): T
  def fromSyzygy(x: scas.polynomial.tree.Polynomial.Element[C, N]): T
}

object PolynomialWithSyzygy {
  trait Element[T <: Element[T, C, N], C, N] extends Polynomial.Element[T, C, N] { this: T =>
    val factory: PolynomialWithSyzygy[T, C, N]
    val syzygy: Module.Element[scas.polynomial.tree.Polynomial.Element[C, N]]
  }
}
