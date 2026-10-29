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
    mods = gb(false, false, false)((mods ++ s)*)
  }
  @targetName("fromCoefficient") def apply(x: C) = ring(x)
  def apply(x: T) = ring.remainder(x)(mods*)
  def unapply(x: T) = Some(x)
  def fromRing(x: T) = x
  def characteristic = ring.characteristic
  def gb(xs: T*): List[T] = gb(false)(xs*)
  def gb(tail: Boolean)(xs: T*): List[T] = gb(false, false, false, tail)(xs*)
  def gb(fussy: Boolean, tail: Boolean)(xs: T*): List[T] = gb(false, true, fussy, tail)(xs*)
  def gb(dummy: Boolean, sugar: Boolean, fussy: Boolean, tail: Boolean)(xs: T*): List[T] = gb(sugar, fussy, tail)((mods ++ xs)*).filter(!_.isZero)
  def gb(sugar: Boolean, fussy: Boolean, tail: Boolean)(xs: T*) = if sugar then ring.gb(fussy, tail)(xs*) else ring.gb(tail)(xs*)
  extension (xs: List[T]) {
    def reduce(dummy: Boolean): List[T] = xs.reduce(xs*)
    def reduce(ys: T*): List[T] = ring.reduce(xs)((mods ++ ys)*)
  }
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
