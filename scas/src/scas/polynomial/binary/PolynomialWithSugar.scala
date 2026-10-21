package scas.polynomial.binary

import scala.compiletime.deferred
import scas.polynomial.gb.SugarEngine
import scas.polynomial.BinaryPolynomial
import scas.power.compact.binary.PowerProduct
import scas.polynomial.PolynomialWithSugar.Element

trait PolynomialWithSugar[T, C] extends scas.polynomial.PolynomialWithSugar[T, C, Array[Int]] with BinaryPolynomial[Element[T], C] {
  given factory: Polynomial[T, C] = deferred
  override given pp: PowerProduct = factory.pp
  override def gb(fussy: Boolean, tail: Boolean)(xs: Element[T]*) = new SugarEngine(fussy, tail)(using pp.relaxed, this).gb(xs*)
}
