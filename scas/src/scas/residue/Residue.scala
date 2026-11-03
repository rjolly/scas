package scas.residue

import scala.annotation.targetName
import scala.compiletime.deferred
import scala.reflect.ClassTag
import scas.util.Conversion
import scas.polynomial.Polynomial
import scas.module.ArrayModule
import scas.prettyprint.Show.given

trait Residue[T : ClassTag, C, M] extends scas.structure.Residue[T, T] {
  given ring: Polynomial[T, C, M] = deferred
  var mods = List.empty[T]
  def generator(n: Int) = ring.generator(n)
  def generators = ring.generators
  given coef2poly: [D: Conversion[C]] => (D => T) = ring.coef2poly
  def update(s: T*): Unit = {
    mods = ring.gb(mods ++ s)
  }
  @targetName("fromCoefficient") def apply(x: C) = ring(x)
  def apply(x: T) = ring.remainder(x)(mods*)
  def unapply(x: T) = Some(x)
  def fromRing(x: T) = x
  def characteristic = ring.characteristic
  extension (xs: List[T]) {
    def gb: List[T] = xs.gb(false)
    def gb(tail: Boolean) = ring.gb(mods ++ xs)(tail).filter(!_.isZero)
    def suggb(fussy: Boolean, tail: Boolean) = ring.suggb(mods ++ xs)(fussy, tail).filter(!_.isZero)
    def siggb(fussy: Boolean, tail: Boolean) = ring.siggb(mods ++ xs)(fussy, tail).filter(!_.isZero)
    def reduce(dummy: Boolean): List[T] = xs.reduce(xs*)
    def reduce(ys: T*): List[T] = ring.reduce(xs)((mods ++ ys)*)
  }
  override def toString = s"${ring}(${mods.show(false)})"
  def toMathML = s"<msub>${ring.toMathML}<mfenced>${mods.toMathML(false)}</mfenced></msub>"

  extension (ring: Polynomial[T, C, M]) def apply(s: T*) = {
    same(s*)
    this
  }
  def same(s: T*): Unit = {
    given ArrayModule[T] = ArrayModule(this)(mods.size)
    assert (s.toArray >< mods.toArray)
  }
}
