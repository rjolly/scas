package scas.residue

import scala.reflect.ClassTag
import scala.compiletime.deferred
import scas.structure.commutative.Field
import scas.polynomial.ufd.PolynomialWithModInverse
import scas.util.{Conversion, unary_~}
import scas.variable.Variable

trait ResidueOverField[T, C, M] extends Residue[T, C, M] with Field[T] {
  given ring: PolynomialWithModInverse[T, C, M] = deferred
  def sqrt[U: Conversion[T]](x: U): T = sqrt(~x)
  def sqrt(x: T) = {
    val n = ring.pp.variables.indexOf(Variable.sqrt(x))
    assert (n > -1)
    generator(n)
  }
  def inverse(x: T) = x.modInverse(mods*)

  extension (ring: PolynomialWithModInverse[T, C, M]) def apply(s: T*) = {
    same(s*)
    this
  }
}

object ResidueOverField {
  class Conv[T : ClassTag, C, M](using PolynomialWithModInverse[T, C, M])(s: T*) extends ResidueOverField[T, C, M] with Field.Conv[T] {
    given instance: Conv[T, C, M] = this
    update(s*)
  }
}
