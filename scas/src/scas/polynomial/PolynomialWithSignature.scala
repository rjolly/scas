package scas.polynomial

import scala.annotation.targetName
import scala.compiletime.deferred
import scas.polynomial.gb.SignatureEngine
import scas.structure.Ring
import scas.power.{PowerProduct, POT}
import PolynomialWithSignature.Element

trait PolynomialWithSignature[T, C, M] extends Polynomial[Element[T, M], C, M] {
  def dimension: Int
  given factory: PolynomialWithGB[T, C, M] = deferred
  override given ring: Ring[C] = factory.ring
  given module: POT[M] = new POT(pp)("e", dimension)
  import module.{max, pp2pot}
  def apply(p: T, n: Int) = (p, module.generator(pp.nbvars + n))
  def apply(s: (M, C)*) = this(factory(s*))
  def apply(p: T) = (p, module.one)
  override def normalize(x: Element[T, M]) = {
    val (p, e) = x
    (factory.normalize(p), e)
  }
  extension (x: Element[T, M]) {
    def underlying = {
      val (p, _) = x
      p
    }
    def signature = {
      val (_, e) = x
      e
    }
    def iterator = x.underlying.iterator
    def size = x.underlying.size
    def head = x.underlying.head
    def last = x.underlying.last
    def add(y: Element[T, M]) = {
      val (p, e) = x
      val (q, f) = y
      (p + q, max(e, f))
    }
    @targetName("ppMultiplyRight") override def %* (m: M) = {
      val (p, e) = x
      (p%* m, e * m)
    }
    override def multiply(m: M, c: C) = {
      val (p, e) = x
      (p.multiply(m, c), e * m)
    }
    def map(f: (M, C) => (M, C)) = {
      val (p, e) = x
      (p.map(f), e)
    }
  }
  def gb(fussy: Boolean, tail: Boolean)(xs: Element[T, M]*) = new SignatureEngine(fussy, tail)(this).gb(xs*)
  @targetName("signatureGB") def gb(fussy: Boolean, tail: Boolean)(xs: T*): List[T] = gb(fussy, tail)(xs.zipWithIndex.map((x, n) => this(x, n))*).map(_.underlying)
}

object PolynomialWithSignature {
  type Element[T, M] = (T, POT.Element[M])
}
