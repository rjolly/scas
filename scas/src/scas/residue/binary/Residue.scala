package scas.residue.binary

import scala.compiletime.deferred
import scas.polynomial.binary.Polynomial

trait Residue[T, C] extends scas.residue.Residue[T, C, Array[Int]] {
  given ring: Polynomial[T, C] = deferred
  override def gb(dummy: Boolean, sugar: Boolean, fussy: Boolean, tail: Boolean)(xs: T*) = super.gb(dummy, sugar, fussy, tail)(xs*).filter(!_.isDefining)
}
