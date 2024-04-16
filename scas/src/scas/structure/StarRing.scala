package scas.structure

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
}
