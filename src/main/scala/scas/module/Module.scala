package scas.module

import scala.reflect.ClassTag
import scas.structure.Ring
import Module.Element

trait Module[R] extends ArrayModule[Element[R], R] {
  val self = this
  def apply(value: Array[R]) = new Element(value)(this)
}

object Module {
  def apply[R](name: String, dimension: Int, ring: Ring[R])(implicit m: ClassTag[Element[R]], cm: ClassTag[R]) = new ModuleImpl(dimension, Some(name), ring)
  def apply[R](dimension: Int, ring: Ring[R])(implicit m: ClassTag[Element[R]], cm: ClassTag[R]) = new ModuleImpl(dimension, None, ring)

  class Element[R](val value: Array[R])(val factory: Module[R]) extends ArrayModule.Element[Element[R], R]
  object Element extends ExtraImplicits

  trait ExtraImplicits {
    implicit def ring2scalar[S <% R, R: Module](value: S) = implicitly[Module[R]].scalar(value)
  }
}
