package scas.residue.binary

import scala.compiletime.deferred
import scas.polynomial.binary.Polynomial

trait Residue[T, C] extends scas.residue.Residue[T, C, Array[Int]] {
  given ring: Polynomial[T, C] = deferred
  override def gb(tail: Boolean)(xs: T*) = super.gb(tail)(xs*).filter(!_.isDefining)
  override def suggb(fussy: Boolean, tail: Boolean)(xs: T*) = super.suggb(fussy, tail)(xs*).filter(!_.isDefining)
  override def siggb(fussy: Boolean, tail: Boolean)(xs: T*) = super.siggb(fussy, tail)(xs*).filter(!_.isDefining)
}
