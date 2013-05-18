package scas.polynomial

import scas.gb.Sugar
import scas.Implicits.infixRingOps
import PolynomialWithSugar.Element

trait PolynomialWithSugar[T <: Element[T, C, N], C, N] extends PolynomialWithGB[T, C, N] with Sugar[T, C, N] {
  abstract override def plus(x: T, y: T) = apply(super.plus(x, y), scala.math.max(x.sugar, y.sugar))
  override def times(x: T, m: Array[N]) = apply(super.times(x, m), x.sugar + pp.degree(m))
  override def subtract(x: T, m: Array[N], c: C, y: T) = apply(super.subtract(x, m, c, y), scala.math.max(x.sugar, y.sugar + pp.degree(m)))
  override def multiply(x: T, m: Array[N], c: C) = apply(super.multiply(x, m, c), x.sugar + pp.degree(m))
  override def multiply(x: T, c: C) = apply(super.multiply(x, c), x.sugar)
  override def divide(x: T, c: C) = apply(super.divide(x, c), x.sugar)
  abstract override def apply(s: (Array[N], C)*) = apply(super.apply(s: _*))
  def apply(x: T): T = apply(x, degree(x))
  def apply(x: T, sugar: Long): T
  def sugar(x: T) = x.sugar
}

object PolynomialWithSugar {
  trait Element[T <: Element[T, C, N], C, N] extends PolynomialOverUFD.Element[T, C, N] { this: T =>
    val factory: PolynomialWithSugar[T, C, N]
    val sugar: Long
  }
}
