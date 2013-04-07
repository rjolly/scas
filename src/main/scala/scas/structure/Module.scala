package scas.structure

import Module.{Element, Scalar}

trait Module[T <: Element[T, R], R] extends AbelianGroup[T] {
  implicit val ring: Ring[R]
  def rtimes(x: T, y: R): T
  def ltimes(x: R, y: T): T
  def scalar(lhs: R) = new Scalar(lhs)(this)
}

object Module {
  trait Element[T <: Element[T, R], R] extends AbelianGroup.Element[T] with (Int => R) { this: T =>
    val factory: Module[T, R]
    def *(rhs: R) = factory.rtimes(lhs, rhs)
  }
  class Scalar[T <: Element[T, R], R](lhs: R)(implicit factory: Module[T, R]) {
    def *(rhs: T) = factory.ltimes(lhs, rhs)
  }
}
