package scas.residue

import scala.annotation.targetName
import scala.compiletime.deferred
import scala.reflect.ClassTag
import scas.util.Conversion
import scas.polynomial.PolynomialWithGB
import scas.module.ArrayModule
import scas.prettyprint.Show.given

trait Residue[T : ClassTag, C, M] extends scas.structure.Residue[T, T] {
  given ring: PolynomialWithGB[T, C, M] = deferred
  var mods = List.empty[T]
  def generator(n: Int) = ring.generator(n)
  def generators = ring.generators
  given coef2poly: [D: Conversion[C]] => (D => T) = ring.coef2poly
  def update(s: T*): Unit = {
    mods = _gb(s*)
  }
  @targetName("fromCoefficient") def apply(x: C) = ring(x)
  def apply(x: T) = ring.remainder(x)(mods*)
  def unapply(x: T) = Some(x)
  def fromRing(x: T) = x
  def characteristic = ring.characteristic
  def _gb(xs: T*) = ring.gb((xs ++ mods)*)
  def gb(xs: T*) = _gb(xs*).filter(!_.isZero)
  def _gb(fussy: Boolean)(xs: T*) = ring.gb(fussy)((xs ++ mods)*)
  def gb(fussy: Boolean)(xs: T*) = _gb(fussy)(xs*).filter(!_.isZero)
  override def toString = s"${ring}(${mods.show(false)})"
  def toMathML = s"<msub>${ring.toMathML}<mfenced>${mods.toMathML(false)}</mfenced></msub>"

  extension (ring: PolynomialWithGB[T, C, M]) def apply(s: T*) = {
    same(s*)
    this
  }
  def same(s: T*): Unit = {
    given ArrayModule[T] = ArrayModule(this)(mods.size)
    assert (s.toArray >< mods.toArray)
  }
}
