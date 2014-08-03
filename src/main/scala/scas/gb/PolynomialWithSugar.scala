package scas.gb

import scala.math.Ordering
import scas.polynomial.PolynomialOverUFD
import scas.Implicits.infixRingOps
import PolynomialWithSugar.Element

trait PolynomialWithSugar[T <: Element[T, C, N], C, N] extends PolynomialWithGM[T, C, N] {
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

  type P = Pair

  class Pair(i: Int, j: Int) extends super.Pair(i, j) { this: P =>
    def skey = (s, scm, j, i)
    val s = scala.math.max(sugar(i) - degree(i), sugar(j) - degree(j)) + pp.degree(scm)
    override def toString = "{" + i + ", " + j + "}, " + s + ", " + reduction
  }

  override def ordering = Ordering by { pair: P => pair.skey }

  def apply(i: Int, j: Int) = new Pair(i, j)
  def degree(i: Int): Long = pp.degree(headPowerProduct(i))
  def sugar(i: Int): Long = sugar(polys(i))
}

object PolynomialWithSugar {
  trait Element[T <: Element[T, C, N], C, N] extends PolynomialOverUFD.Element[T, C, N] { this: T =>
    val factory: PolynomialWithSugar[T, C, N]
    val sugar: Long
  }
}
