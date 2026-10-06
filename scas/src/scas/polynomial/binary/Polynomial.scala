package scas.polynomial.binary

import scas.polynomial.gb.GBEngine
import scas.polynomial.BinaryPolynomial
import Polynomial.WithSugar

trait Polynomial[T, C] extends scas.polynomial.PolynomialWithGB[T, C, Array[Int]] with BinaryPolynomial[T, C] {
  override def gb(xs: T*) = new GBEngine(using pp.defining, this).gb(xs*)
  override def sugar = new WithSugar(using this)
}

object Polynomial {
  class WithSugar[T, C](using Polynomial[T, C]) extends PolynomialWithSugar[T, C]
}
