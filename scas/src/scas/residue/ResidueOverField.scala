package scas.residue

import scala.reflect.ClassTag
import scas.util.{Conversion, unary_~}
import scas.structure.commutative.Field
import scas.polynomial.PolynomialOverField
import scas.variable.Variable

abstract class ResidueOverField[T : ClassTag, C, M] extends Residue[T, C, M] with Field[T] {
  given ring: PolynomialOverField[T, C, M]
  def sqrt[U: Conversion[T]](x: U): T = sqrt(~x)
  def sqrt(x: T) = {
    val n = ring.variables.indexOf(Variable.sqrt(x))
    assert (n > -1)
    ring.generator(n)
  }
  def inverse(x: T) = x.modInverse(mods*)

  extension (ring: Field[T]) def apply(s: T*): ResidueOverField[T, C, M] = {
    same(s*)
    this
  }
}
