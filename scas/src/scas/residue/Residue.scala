package scas.residue

import scala.reflect.ClassTag
import scas.util.{Conversion, unary_~}
import scas.structure.commutative.{Field, UniqueFactorizationDomain}
import scas.polynomial.PolynomialOverField
import scas.module.ArrayModule
import scas.variable.Variable

abstract class Residue[T : ClassTag, C, M] extends scas.structure.commutative.Residue[T, T] with Field[T] {
  given ring: PolynomialOverField[T, C, M]
  var list = List.empty[T]
  def generators = ring.generators
  given coef2poly[D: Conversion[C]]: (D => T) = ring.coef2poly
  def update(mod: T): Unit = {
    // assert mod is irreducible
    list = List(mod)
  }
  def generator = ring.generator
  def variables = ring.variables
  def sqrt[U: Conversion[T]](x: U): T = sqrt(~x)
  def sqrt(x: T) = {
    val n = variables.indexOf(Variable.sqrt(x))
    assert (n > -1)
    generator(n)
  }
  def apply(x: T) = x.reduce(list)
  extension (x: T) def unapply = x
  def fromRing(x: T) = x
  def characteristic = ring.characteristic
  def inverse(x: T) = x.modInverse(list(0))
  override def toString = s"${ring}(${list.show(false)})"
  def toMathML = s"<msub>${ring.toMathML}<mfenced>${list.toMathML(false)}</mfenced></msub>"

  extension (ring: UniqueFactorizationDomain[T]) def apply(s: T*) = {
    given ArrayModule[T] = ArrayModule(this)(list.size)
    assert (s.toArray >< list.toArray)
    this
  }
}

object Residue {
  def apply[T : ClassTag, C, M](ring: PolynomialOverField[T, C, M])(mod: T) = new conversion.Residue(using ring)(mod)
}
