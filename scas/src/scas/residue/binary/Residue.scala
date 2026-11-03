package scas.residue.binary

import scala.compiletime.deferred
import scas.polynomial.binary.Polynomial

trait Residue[T, C] extends scas.residue.Residue[T, C, Array[Int]] {
  given ring: Polynomial[T, C] = deferred
  extension (xs: List[T]) {
    override def gb(tail: Boolean) = super.gb(xs)(tail).filter(!_.isDefining)
    override def suggb(fussy: Boolean, tail: Boolean) = super.suggb(xs)(fussy, tail).filter(!_.isDefining)
    override def siggb(fussy: Boolean, tail: Boolean) = super.siggb(xs)(fussy, tail).filter(!_.isDefining)
  }
}
