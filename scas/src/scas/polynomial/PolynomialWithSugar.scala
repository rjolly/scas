package scas.polynomial

import scala.annotation.targetName
import scala.compiletime.deferred
import scas.polynomial.gb.SugarEngine
import scas.power.PowerProduct
import scas.structure.Ring
import scas.base.BigInteger
import BigInteger.{max, given}
import PolynomialWithSugar.Element

trait PolynomialWithSugar[T, C, M] extends Polynomial[Element[T], C, M] {
  given factory: Polynomial[T, C, M] = deferred
  override given ring: Ring[C] = factory.ring
  def apply(s: (M, C)*) = this(factory(s*))
  @targetName("fromPolynomial") def apply(p: T) = (p, p.degree)
  override def normalize(x: Element[T]) = {
    val (p, e) = x
    (factory.normalize(p), e)
  }
  extension (x: Element[T]) {
    def underlying = {
      val (p, _) = x
      p
    }
    def sugar = {
      val (_, e) = x
      e
    }
    def iterator = x.underlying.iterator
    def size = x.underlying.size
    def head = x.underlying.head
    def last = x.underlying.last
    def add(y: Element[T]) = {
      val (p, e) = x
      val (q, f) = y
      (p + q, max(e, f))
    }
    @targetName("ppMultiplyRight") override def %* (m: M) = {
      val (p, e) = x
      (p%* m, e + m.degree)
    }
    override def multiply(m: M, c: C) = {
      val (p, e) = x
      (p.multiply(m, c), e + m.degree)
    }
    def map(f: (M, C) => (M, C)) = {
      val (p, e) = x
      (p.map(f), e)
    }
  }
  override def gb(xs: Element[T]*) = gb(false)(xs*)
  def gb(fussy: Boolean)(xs: Element[T]*) = new SugarEngine(fussy)(this).gb(xs*)
  @targetName("sugarGB") def gb(fussy: Boolean)(xs: T*): List[T] = gb(fussy)(xs.map(this(_))*).map(_.underlying)
}

object PolynomialWithSugar {
  type Element[T] = (T, BigInteger)
}
