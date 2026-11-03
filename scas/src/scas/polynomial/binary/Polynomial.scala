package scas.polynomial.binary

import scas.polynomial.gb.GBEngine
import scas.polynomial.BinaryPolynomial
import Polynomial.{WithSugar, WithSignature}

trait Polynomial[T, C] extends scas.polynomial.Polynomial[T, C, Array[Int]] with BinaryPolynomial[T, C] {
  extension (xs: List[T]) override def gb(tail: Boolean) = new GBEngine(tail)(using pp.relaxed, this).gb(xs*)
  override def sugar = new WithSugar(using this)
  override def signature(dimension: Int) = new WithSignature(dimension)(using this)
}

object Polynomial {
  class WithSugar[T, C](using Polynomial[T, C]) extends PolynomialWithSugar[T, C]
  class WithSignature[T, C](val dimension: Int)(using Polynomial[T, C]) extends PolynomialWithSignature[T, C]
}
