package scas.structure

import Module.Element

trait VectorSpace[T <: Element[T, R], R] extends Module[T, R] {
  implicit val ring: Field[R]
}
