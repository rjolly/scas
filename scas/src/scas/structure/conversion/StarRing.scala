package scas.structure.conversion

import scas.util.{Conversion, unary_~}
import scas.base.BigInteger
import BigInteger.given

trait StarRing[T] extends scas.structure.StarRing[T] with Ring[T] {
  abstract override def real(x: T) = super.real(x)
  def real[U: Conversion[T]](x: U): T = real(~x)
  abstract override def imag(x: T) = super.imag(x)
  def imag[U: Conversion[T]](x: U): T = imag(~x)
  abstract override def conjugate(x: T) = super.conjugate(x)
  def conjugate[U: Conversion[T]](x: U): T = conjugate(~x)
  def magnitude2(x: T) = real(x)\2 + imag(x)\2
  def magnitude2[U: Conversion[T]](x: U): T = magnitude2(~x)
}
