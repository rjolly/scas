package scas.structure.impl

trait StarRing[T] extends Ring[T] {
  def real(x: T): T
  def imag(x: T): T
  extension (x: T) {
    def isReal = imag(x) >< zero
    def isImag = real(x) >< zero
  }
  def conjugate(x: T): T
  def magnitude2(x: T): T
}
