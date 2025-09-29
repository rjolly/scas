package scas.residue

import scala.annotation.targetName
import scala.compiletime.deferred
import scala.reflect.ClassTag
import scas.util.Conversion
import scas.polynomial.ufd.{PolynomialWithGB, PolynomialOverFieldWithGB}
import scas.module.ArrayModule
import scas.prettyprint.Show.given

trait Residue[T : ClassTag, C, N] extends scas.structure.commutative.Residue[T, T] {
  given ring: PolynomialWithGB[T, C, N] = deferred
  var mods = List.empty[T]
  def generator(n: Int) = ring.generator(n)
  def generators = ring.generators
  given coef2poly: [D: Conversion[C]] => (D => T) = ring.coef2poly
  def update(s: T*): Unit = {
    mods = gb(false, s*)
  }
  @targetName("fromCoefficient") def apply(x: C) = ring(x)
  def apply(x: T) = ring.remainder(x)(mods*)
  def unapply(x: T) = Some(x)
  def fromRing(x: T) = x
  def characteristic = ring.characteristic
  def gb(dummy: Boolean, xs: T*) = ring.gb((xs ++ mods)*)
  def gb(xs: T*): List[T] = gb(false, xs*).filter(!_.isZero)
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
  def apply[T : ClassTag, C, N](ring: PolynomialOverFieldWithGB[T, C, N])(s: T*) = new ResidueOverField.Conv(using ring)(s*)
}
