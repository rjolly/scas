package scas.residue

import scala.reflect.ClassTag
import scas.util.Conversion
import scas.polynomial.{PolynomialOverUFD, PolynomialWithModInverse}
import scas.module.ArrayModule

abstract class Residue[T : ClassTag, C, M] extends scas.structure.commutative.Residue[T, T] {
  given ring: PolynomialOverUFD[T, C, M]
  var mods = List.empty[T]
  def generators = ring.generators
  given coef2poly[D: Conversion[C]]: (D => T) = ring.coef2poly
  def update(s: T*): Unit = {
    mods ++= s
  }
  def apply(x: T) = ring.remainder(x)(mods)
  extension (x: T) def unapply = x
  def fromRing(x: T) = x
  def characteristic = ring.characteristic
  override def toString = s"${ring}(${mods.show(false)})"
  def toMathML = s"<msub>${ring.toMathML}<mfenced>${mods.toMathML(false)}</mfenced></msub>"

  extension (ring: PolynomialOverUFD[T, C, M]) def apply(s: T*) = {
    same(s*)
    this
  }
  def same(s: T*): Unit = {
    given ArrayModule[T] = ArrayModule(this)(mods.size)
    assert (s.toArray >< mods.toArray)
  }
}

object Residue {
  def apply[T : ClassTag, C, M](ring: PolynomialWithModInverse[T, C, M])(s: T*) = new conversion.ResidueOverField(using ring)(s*)
}
