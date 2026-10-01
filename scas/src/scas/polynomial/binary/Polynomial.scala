package scas.polynomial.binary

import scas.polynomial.gb.GBEngine
import scas.polynomial.BinaryPolynomial

trait Polynomial[T, C] extends scas.polynomial.gb.Polynomial[T, C, Array[Int]] with BinaryPolynomial[T, C] {
  override def gb(xs: T*) = new GBEngine(using pp.defining, this).gb(xs*)
  override def sugar = new PolynomialWithSugar(using this)
}
