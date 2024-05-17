package scas.structure

import scas.util.{Conversion, unary_~}
import scas.base.BigInteger
import BigInteger.given

trait StarRing[T] extends Ring[T] {
  def real(x: T): T
  def imag(x: T): T
  extension (x: T) {
    def isReal = imag(x) >< zero
    def isImag = real(x) >< zero
  }
  def conjugate(x: T): T
  def magnitude2(x: T) = real(x)\2 + imag(x)\2
  def real[U: Conversion[T]](x: U): T = real(~x)
  def imag[U: Conversion[T]](x: U): T = imag(~x)
  def conjugate[U: Conversion[T]](x: U): T = conjugate(~x)
  def magnitude2[U: Conversion[T]](x: U): T = magnitude2(~x)
}
