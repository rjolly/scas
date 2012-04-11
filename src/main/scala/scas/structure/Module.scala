package scas.structure

import Module.Element

trait Module[T <: Element[T, R], R] extends AbelianGroup[T] {
  implicit val ring: Ring[R]
  def rtimes(x: T, y: R): T
  def ltimes(x: R, y: T): T

  class Scalar(val value: R) {
    def *(rhs: T) = ltimes(value, rhs)
  }
  def scalar(value: R): Scalar = new Scalar(value)
}

object Module {
  trait Element[T <: Element[T, R], R] extends AbelianGroup.Element[T] { this: T =>
    val factory: Module[T, R]
    def *(value: R) = factory.rtimes(this, value)
  }
}
