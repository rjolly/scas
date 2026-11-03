package scas.polynomial.mutable

import scas.power.PowerProduct
import Polynomial.{WithSugar, WithSignature}

trait Polynomial[T, C, M] extends scas.polynomial.Polynomial[T, C, M] {
  def unmodifiable(x: T): T
  def modifiable(x: T): T
  extension (x: T) {
    override def subtract(y: T) = unmodifiable(super.subtract(modifiable(x))(y))

    override def multiplyAndAdd(y: T, z: T) = unmodifiable(super.multiplyAndAdd(modifiable(x))(y, z))

    override def reduce(ys: T*) = unmodifiable(super.reduce(modifiable(x))(ys*))

    override def reduce(strict: Boolean, tail: Boolean, ys: T*) = unmodifiable(super.reduce(modifiable(x))(strict, tail, ys*))

    override def reduce(m: M, a: C, y: T, b: C) = unmodifiable(super.reduce(modifiable(x))(m, a, y, b))

    override def subtract(m: M, c: C, y: T) = super.subtract(x)(m, c, y)
  }
  override def sugar = new WithSugar(using this)
  override def signature(dimension: Int) = new WithSignature(dimension)(using this)
}

object Polynomial {
  class WithSugar[T, C, M](using Polynomial[T, C, M]) extends PolynomialWithSugar[T, C, M] {
    override given pp: PowerProduct[M] = factory.pp
  }
  class WithSignature[T, C, M](val dimension: Int)(using Polynomial[T, C, M]) extends PolynomialWithSignature[T, C, M] {
    override given pp: PowerProduct[M] = factory.pp
  }
}
