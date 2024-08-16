package scas.residue

import scala.reflect.ClassTag
import scas.util.{Conversion, unary_~}
import scas.structure.commutative.{Field, UniqueFactorizationDomain}
import scas.polynomial.PolynomialOverField
import scas.module.ArrayModule
import scas.variable.Variable

abstract class Residue[T : ClassTag, C, M] extends scas.structure.commutative.Residue[T, T] with Field[T] {
  given ring: PolynomialOverField[T, C, M]
  var mods = List.empty[T]
  def generators = ring.generators
  given coef2poly[D: Conversion[C]]: (D => T) = ring.coef2poly
  def update(s: T*): Unit = {
    // assert mod is irreducible
    mods ++= s
  }
  def sqrt[U: Conversion[T]](x: U): T = sqrt(~x)
  def sqrt(x: T) = {
    val n = ring.variables.indexOf(Variable.sqrt(x))
    assert (n > -1)
    ring.generator(n)
  }
  def apply(x: T) = ring.remainder(x)(mods)
  extension (x: T) def unapply = x
  def fromRing(x: T) = x
  def characteristic = ring.characteristic
  def inverse(x: T) = x.modInverse(mods*)
  override def toString = s"${ring}(${mods.show(false)})"
  def toMathML = s"<msub>${ring.toMathML}<mfenced>${mods.toMathML(false)}</mfenced></msub>"

  extension (ring: UniqueFactorizationDomain[T]) def apply(s: T*) = {
    given ArrayModule[T] = ArrayModule(this)(mods.size)
    assert (s.toArray >< mods.toArray)
    this
  }
}

object Residue {
  def apply[T : ClassTag, C, M](ring: PolynomialOverField[T, C, M])(s: T*) = new conversion.Residue(using ring)(s*)
}
