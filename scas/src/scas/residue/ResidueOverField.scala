package scas.residue

import scala.reflect.ClassTag
import scas.util.{Conversion, unary_~}
import scas.structure.commutative.Field
import scas.polynomial.PolynomialOverFieldWithGB
import scas.variable.Variable

class ResidueOverField[T : ClassTag, C, N](using var ring: PolynomialOverFieldWithGB[T, C, N])(s: T*) extends Residue[T, C, N] with Field[T] {
  update(s*)
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
