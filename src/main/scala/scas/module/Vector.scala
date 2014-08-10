package scas.module

import scala.reflect.ClassTag
import scas.structure.{Field, VectorSpace}
import Vector.Element

trait Vector[R] extends ArrayModule[Element[R], R] with VectorSpace[Element[R], R] {
  val self = this
  def apply(value: Array[R]) = new Element(value)(this)
}

object Vector {
  def apply[R](name: String, dimension: Int, ring: Field[R])(implicit m: ClassTag[Element[R]], cm: ClassTag[R]) = new VectorImpl(dimension, Some(name), ring)
  def apply[R](dimension: Int, ring: Field[R])(implicit m: ClassTag[Element[R]], cm: ClassTag[R]) = new VectorImpl(dimension, None, ring)

  class Element[R](val value: Array[R])(val factory: Vector[R]) extends ArrayModule.Element[Element[R], R] with VectorSpace.Element[Element[R], R]
  object Element extends ExtraImplicits

  trait ExtraImplicits {
    implicit def ring2vectorScalar[S <% R, R: Vector](value: S) = implicitly[Vector[R]].scalar(value)
  }
}
