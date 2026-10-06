package scas.polynomial.mutable

import scas.power.PowerProduct
import PolynomialWithGB.WithSugar

trait PolynomialWithGB[T, C, M] extends scas.polynomial.PolynomialWithGB[T, C, M] with Polynomial[T, C, M] {
  override def sugar = new WithSugar(using this)
}

object PolynomialWithGB {
  class WithSugar[T, C, M](using PolynomialWithGB[T, C, M]) extends PolynomialWithSugar[T, C, M] {
    override given pp: PowerProduct[M] = factory.pp
  }
}
