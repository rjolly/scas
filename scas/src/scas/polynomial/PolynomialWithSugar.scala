package scas.polynomial

import scala.annotation.targetName
import scala.compiletime.deferred
import scas.power.PowerProduct
import scas.structure.Ring
import PolynomialWithSugar.Element
import scas.base.BigInteger
import BigInteger.{max, given}

trait PolynomialWithSugar[T, C, M] extends Polynomial[Element[T], C, M] {
  given factory: Polynomial[T, C, M] = deferred
  override given ring: Ring[C] = factory.ring
  override given pp: PowerProduct[M] = factory.pp
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
}

object PolynomialWithSugar {
  type Element[T] = (T, BigInteger)
}
