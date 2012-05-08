package scas.structure

import Module.{Element, Scalar}

trait Module[T <: Element[T, R], R] extends AbelianGroup[T] { outer =>
  implicit val ring: Ring[R]
  def rtimes(x: T, y: R): T
  def ltimes(x: R, y: T): T
  def apply(value: R): Scalar[T, R] = new Scalar[T, R] {
    val lhs = value
    val factory = outer
  }
}

object Module {
  trait Element[T <: Element[T, R], R] extends AbelianGroup.Element[T] with Ops[T, R] { this: T =>
    def apply(n: Int): R
  }
  trait Ops[T <: Element[T, R], R] extends AbelianGroup.Ops[T] {
    val factory: Module[T, R]
    def *(rhs: R) = factory.rtimes(lhs, rhs)
  }
  trait Scalar[T <: Element[T, R], R] {
    val lhs: R
    val factory: Module[T, R]
    def *(rhs: T) = factory.ltimes(lhs, rhs)
  }
}
