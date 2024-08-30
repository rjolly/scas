package scas.residue

import scala.reflect.ClassTag
import scas.util.{Conversion, unary_~}
import scas.structure.commutative.Field
import scas.polynomial.PolynomialOverFieldWithGB
import scas.variable.Variable

abstract class ResidueOverField[T : ClassTag, C, N] extends Residue[T, C, N] with Field[T] {
  given ring: PolynomialOverFieldWithGB[T, C, N]
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
