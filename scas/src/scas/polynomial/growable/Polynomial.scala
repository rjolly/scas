package scas.polynomial.growable

import scala.compiletime.deferred
import scas.structure.Ring
import scas.power.growable.PowerProduct
import scas.variable.Variable

trait Polynomial[T, C, M] extends scas.polynomial.Polynomial[T, C, M] {
  given pp: PowerProduct[M] = deferred
  def extend(variables: Variable*): Unit = pp.extend(variables*)

  extension (ring: Ring[C]) override def apply(s: T*): Polynomial[T, C, M] = {
    same(s*)
    this
  }
}
