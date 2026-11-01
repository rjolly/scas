package scas.polynomial.mutable

import scas.power.PowerProduct
import PolynomialWithGB.{WithSugar, WithSignature}

trait PolynomialWithGB[T, C, M] extends scas.polynomial.PolynomialWithGB[T, C, M] with Polynomial[T, C, M] {
  override def sugar = new WithSugar(using this)
  override def signature(dimension: Int) = new WithSignature(dimension)(using this)
}

object PolynomialWithGB {
  class WithSugar[T, C, M](using PolynomialWithGB[T, C, M]) extends PolynomialWithSugar[T, C, M] {
    override given pp: PowerProduct[M] = factory.pp
  }
  class WithSignature[T, C, M](val dimension: Int)(using PolynomialWithGB[T, C, M]) extends PolynomialWithSignature[T, C, M] {
    override given pp: PowerProduct[M] = factory.pp
  }
}
