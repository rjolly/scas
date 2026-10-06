package scas.polynomial

import scas.power.PowerProduct
import PolynomialWithGB.WithSugar

trait PolynomialWithGB[T, C, M] extends Polynomial[T, C, M] {
  def gb(fussy: Boolean)(xs: T*) = sugar.gb(fussy)(xs*)
  def sugar: PolynomialWithSugar[T, C, M] = new WithSugar(using this)
}

object PolynomialWithGB {
  class WithSugar[T, C, M](using PolynomialWithGB[T, C, M]) extends PolynomialWithSugar[T, C, M] {
    override given pp: PowerProduct[M] = factory.pp
  }
}
