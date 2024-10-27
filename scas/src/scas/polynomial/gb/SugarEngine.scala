package scas.polynomial.gb

import scala.annotation.targetName
import scas.polynomial.{Polynomial, PolynomialWithSugar}
import PolynomialWithSugar.Element
import scas.base.{BigInteger, Boolean}
import BigInteger.self.{max, given}
import Boolean.self.given
import scala.math.Ordering
import scas.math.Ordering.given

class SugarEngine[T, C, M](fussy: Boolean)(using factory: PolynomialWithSugar[T, C, M]) extends GMSetting[Element[T], C, M, SugarPair] {
  def this(fussy: Boolean)(factory: Polynomial[T, C, M]) = this(fussy)(using PolynomialWithSugar(using factory))
  def this(factory: Polynomial[T, C, M]) = this(false)(factory)
  import factory.pp

  override def ordering = Ordering by { (pair: SugarPair[M]) => pair.skey }

  override def natural = if (fussy) ordering else super.natural

  extension (p1: SugarPair[M]) override def | (p2: SugarPair[M]) = super.|(p1)(p2) && (fussy >> (p1 < p2))

  override def apply(i: Int, j: Int) = {
    val m = i.headPowerProduct
    val n = j.headPowerProduct
    val scm = pp.lcm(m, n)
    val s = max(i.sugar - i.degree, j.sugar - j.degree)
    new SugarPair(i, j, m, n, scm, s + scm.degree)
  }

  extension (i: Int) def degree = i.headPowerProduct.degree
  extension (i: Int) def sugar = {
    val (_, e) = polys(i)
    e
  }

  @targetName("sugarGB") def gb(xs: T*): List[T] = gb(xs.map(factory(_))*).map(_._1)
}
