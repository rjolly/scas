package scas.residue

import scala.annotation.targetName
import scala.reflect.ClassTag
import scas.util.Conversion
import scas.polynomial.{PolynomialWithGB, PolynomialOverFieldWithGB}
import scas.module.ArrayModule

abstract class Residue[T : ClassTag, C, N] extends scas.structure.commutative.Residue[T, T] {
  given ring: PolynomialWithGB[T, C, N]
  var mods = List.empty[T]
  def variables = ring.variables
  def generator(n: Int) = ring.generator(n)
  def generators = ring.generators
  given coef2poly[D: Conversion[C]]: (D => T) = ring.coef2poly
  def update(s: T*): Unit = {
    mods ++= s
  }
  @targetName("fromCoefficient") def apply(x: C) = ring(x)
  def apply(x: T) = ring.remainder(x)(mods)
  extension (x: T) def convert = ring.convert(x)
  extension (x: T) def unapply = x
  def fromRing(x: T) = x
  def characteristic = ring.characteristic
  def gb(xs: T*) = ring.gb((xs ++ mods)*)
  override def toString = s"${ring}(${mods.show(false)})"
  def toMathML = s"<msub>${ring.toMathML}<mfenced>${mods.toMathML(false)}</mfenced></msub>"

  extension (ring: PolynomialWithGB[T, C, N]) def apply(s: T*) = {
    same(s*)
    this
  }
  def same(s: T*): Unit = {
    given ArrayModule[T] = ArrayModule(this)(mods.size)
    assert (s.toArray >< mods.toArray)
  }
}

object Residue {
  def apply[T : ClassTag, C, N](ring: PolynomialOverFieldWithGB[T, C, N])(s: T*) = new conversion.ResidueOverField(using ring)(s*)
}
