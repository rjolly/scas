package scas.polynomial

import scas.power.PowerProduct
import scas.polynomial.gb.GBEngine
import PolynomialWithGB.{WithSugar, WithSignature}

trait PolynomialWithGB[T, C, M] extends Polynomial[T, C, M] {
  def gb(xs: T*): List[T] = gb(false)(xs*)
  def gb(tail: Boolean)(xs: T*) = new GBEngine(tail)(this).gb(xs*)
  def suggb(fussy: Boolean, tail: Boolean)(xs: T*) = sugar.gb(fussy, tail)(xs*)
  def sugar: PolynomialWithSugar[T, C, M] = new WithSugar(using this)
  def siggb(fussy: Boolean, tail: Boolean)(xs: T*) = signature(xs.size).gb(fussy, tail)(xs*)
  def signature(dimension: Int): PolynomialWithSignature[T, C, M] = new WithSignature(dimension)(using this)
}

object PolynomialWithGB {
  class WithSugar[T, C, M](using PolynomialWithGB[T, C, M]) extends PolynomialWithSugar[T, C, M] {
    override given pp: PowerProduct[M] = factory.pp
  }
  class WithSignature[T, C, M](val dimension: Int)(using PolynomialWithGB[T, C, M]) extends PolynomialWithSignature[T, C, M] {
    override given pp: PowerProduct[M] = factory.pp
  }
}
