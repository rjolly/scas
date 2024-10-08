package scas.polynomial.gb

import scas.polynomial.PolynomialWithSugar
import PolynomialWithSugar.Element
import scas.base.BigInteger
import BigInteger.{max, given}
import scala.math.Ordering

class SugarEngine[T, C, M](using factory: PolynomialWithSugar[T, C, M]) extends GMSetting[Element[T], C, M, SugarPair] {
  import factory.pp

  override def ordering = Ordering by { (pair: SugarPair[M]) => pair.skey }

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
}
