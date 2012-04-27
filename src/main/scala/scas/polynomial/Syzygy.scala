package scas.polynomial

import scas.Variable
import scas.structure.Ring
import scas.module.Module
import Module.Element

class Syzygy[T <: Polynomial.Element[T, C, N], C, @specialized(Int, Long) N](variables: Array[Variable], override val ring: Polynomial[T, C, N])(cm: ClassManifest[T]) extends Module(variables)(ring, cm) {
  def multiply(w: Element[T], x: Array[N], y: C) = w * ring.fromPowerProduct(x) * ring(y)
  def multiply(w: Element[T], y: C) = w * ring(y)
}

object Syzygy {
  def apply[T <: Polynomial.Element[T, C, N], C, @specialized(Int, Long) N](name: String, dimension: Int, ring: Polynomial[T, C, N])(implicit cm: ClassManifest[T]) = new Syzygy((for (i <- 0 until dimension) yield Variable(name, i)).toArray, ring)(cm)
}
