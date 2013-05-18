package scas.gb

import scala.math.Ordering
import scas.polynomial.PolynomialWithSugar
import PolynomialWithSugar.Element

trait Sugar[T <: Element[T, C, N], C, N] extends GM[T, C, N] { outer: PolynomialWithSugar[T, C, N] =>
  type P <: Pair

  class Pair(i: Int, j: Int) extends super.Pair(i, j) { this: P =>
    def skey = (sugar, scm, j, i)
    val sugar = scala.math.max(outer.sugar(i) - degree(i), outer.sugar(j) - degree(j)) + pp.degree(scm)
    override def toString = "{" + i + ", " + j + "}, " + sugar + ", " + reduction
  }

  override def ordering = Ordering by { pair: P => pair.skey }

  def degree(i: Int): Long = pp.degree(headPowerProduct(i))
  def sugar(i: Int): Long = sugar(polys(i))
}
