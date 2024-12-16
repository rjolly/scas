package scas.residue

import scala.reflect.ClassTag
import scala.compiletime.deferred
import scas.structure.commutative.Field
import scas.polynomial.ufd.PolynomialOverFieldWithGB
import scas.util.{Conversion, unary_~}
import scas.variable.Variable

trait ResidueOverField[T, C, N] extends Residue[T, C, N] with Field[T] {
  given ring: () => PolynomialOverFieldWithGB[T, C, N] = deferred
  def sqrt[U: Conversion[T]](x: U): T = sqrt(~x)
  def sqrt(x: T) = {
    val n = variables.indexOf(Variable.sqrt(x))
    assert (n > -1)
    generator(n)
  }
  def inverse(x: T) = x.modInverse(mods*)

  extension (ring: PolynomialOverFieldWithGB[T, C, N]) def apply(s: T*) = {
    same(s*)
    this
  }
}

object ResidueOverField {
  class Conv[T : ClassTag, C, N](using_ring: PolynomialOverFieldWithGB[T, C, N])(s: T*) extends ResidueOverField[T, C, N] with Field.Conv[T] {
    override given ring: () => PolynomialOverFieldWithGB[T, C, N] = using_ring
    given instance: Conv[T, C, N] = this
    update(s*)
  }
}
