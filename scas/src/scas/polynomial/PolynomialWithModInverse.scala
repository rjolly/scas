package scas.polynomial

import scala.reflect.ClassTag
import scas.structure.commutative.Field

trait PolynomialWithModInverse[T : ClassTag, C, M] extends PolynomialOverField[T, C, M] {
  extension (x: T) def modInverse(mods: T*): T
  extension (ring: Field[C]) override def apply(s: T*): PolynomialWithModInverse[T, C, M] = {
    same(s*)
    this
  }
}
