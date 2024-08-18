package scas.residue

import scala.reflect.ClassTag
import scas.util.{Conversion, unary_~}
import scas.structure.commutative.Field
import scas.polynomial.PolynomialWithModInverse
import scas.variable.Variable

abstract class ResidueOverField[T : ClassTag, C, M] extends Residue[T, C, M] with Field[T] {
  given ring: PolynomialWithModInverse[T, C, M]
  def sqrt[U: Conversion[T]](x: U): T = sqrt(~x)
  def sqrt(x: T) = {
    val n = variables.indexOf(Variable.sqrt(x))
    assert (n > -1)
    generator(n)
  }
  def inverse(x: T) = x.modInverse(mods*)

  extension (ring: PolynomialWithModInverse[T, C, M]) def apply(s: T*) = {
    same(s*)
    this
  }
}
