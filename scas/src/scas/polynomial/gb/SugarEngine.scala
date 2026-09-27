package scas.polynomial.gb

import scala.annotation.targetName
import scas.polynomial.Polynomial
import scas.polynomial.PolynomialWithSugar.Element
import scas.base.{BigInteger, Boolean}
import BigInteger.self.{max, given}
import Boolean.self.given
import scas.math.Ordering
import SugarEngine.Pair

class SugarEngine[T, C, M](fussy: Boolean)(using factory: scas.polynomial.PolynomialWithSugar[T, C, M]) extends GMEngine.Impl[Element[T], C, M, Pair[M]] {
  def this(fussy: Boolean)(factory: Polynomial[T, C, M]) = this(fussy)(using PolynomialWithSugar(using factory))
  def this(factory: Polynomial[T, C, M]) = this(false)(factory)
  import factory.pp

  override def ordering = Ordering by { pair => (pair.s, pair.scm, pair.j, pair.i) }

  override def natural = if fussy then ordering else super.natural

  extension (p1: Pair[M]) override def | (p2: Pair[M]) = super.|(p1)(p2) && (fussy >> (p1 < p2))

  def apply(i: Int, j: Int, reduction: Boolean, principal: Int, coprime: Boolean, scm: M) = new Pair(i, j, reduction, principal, coprime, scm, max(i.sugar - i.degree, j.sugar - j.degree) + scm.degree)

  extension (pair: Pair[M]) def show = "{" + pair.i + ", " + pair.j + "}, " + pair.s.show + ", " + pair.reduction

  extension (i: Int) def degree = i.headPowerProduct.degree
  extension (i: Int) def sugar = polys(i).sugar

  @targetName("sugarGB") def gb(xs: T*): List[T] = gb(xs.map(factory(_))*).map(_._1)
}

object SugarEngine {
  class Pair[M](i: Int, j: Int, reduction: Boolean, principal: Int, coprime: Boolean, scm: M, val s: BigInteger) extends GBEngine.Pair(i, j, reduction, principal, coprime, scm)
}
