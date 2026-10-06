package scas.polynomial.binary.mutable

import Polynomial.WithSugar

trait Polynomial[T, C] extends scas.polynomial.binary.Polynomial[T, C] with scas.polynomial.mutable.PolynomialWithGB[T, C, Array[Int]] {
  override def sugar = new WithSugar(using this)
}

object Polynomial {
  class WithSugar[T, C](using Polynomial[T, C]) extends scas.polynomial.binary.PolynomialWithSugar[T, C] with scas.polynomial.mutable.PolynomialWithSugar[T, C, Array[Int]] {
    override given factory: Polynomial[T, C] = summon
  }
}
