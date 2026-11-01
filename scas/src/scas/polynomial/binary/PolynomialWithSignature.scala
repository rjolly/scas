package scas.polynomial.binary

import scala.compiletime.deferred
import scas.polynomial.gb.SignatureEngine
import scas.polynomial.BinaryPolynomial
import scas.power.compact.binary.PowerProduct
import PolynomialWithSignature.Element

trait PolynomialWithSignature[T, C] extends scas.polynomial.PolynomialWithSignature[T, C, Array[Int]] with BinaryPolynomial[Element[T], C] {
  given factory: Polynomial[T, C] = deferred
  override given pp: PowerProduct = factory.pp
  override def gb(fussy: Boolean, tail: Boolean)(xs: Element[T]*) = new SignatureEngine(fussy, tail)(using pp.relaxed, this).gb(xs*)
}

object PolynomialWithSignature {
  type Element[T] = scas.polynomial.PolynomialWithSignature.Element[T, Array[Int]]
}
