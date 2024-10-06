package scas.polynomial

import scala.annotation.targetName
import scala.reflect.ClassTag
import scas.math.Numeric
import scas.power.ArrayPowerProduct
import scas.structure.commutative.UniqueFactorizationDomain
import PolynomialWithSugar.Element
import scas.base.BigInteger
import BigInteger.{max, given}

class PolynomialWithSugar[T, C, N : Numeric : ClassTag](using factory: PolynomialWithGB[T, C, N])(using ClassTag[T]) extends PolynomialWithGB[Element[T], C, N] {
  given ring: UniqueFactorizationDomain[C] = factory.ring
  given pp: ArrayPowerProduct[N] = factory.pp
  def newInstance(pp: ArrayPowerProduct[N]) = new PolynomialWithSugar(using factory.newInstance(pp))
  def apply(s: (Array[N], C)*) = this(factory(s*))
  @targetName("fromSelf") def apply(p: T) = (p, p.degree)
  extension (x: Element[T]) {
    def iterator = {
      val (p, _) = x
      p.iterator
    }
    def size = {
      val (p, _) = x
      p.size
    }
    def head = {
      val (p, _) = x
      p.head
    }
    def last = {
      val (p, _) = x
      p.last
    }
    def add(y: Element[T]) = {
      val (p, e) = x
      val (q, f) = y
      (p + q, max(e, f))
    }
    override def ppMultiplyRight(m: Array[N]) = {
      val (p, e) = x
      (p.ppMultiplyRight(m), e + m.degree)
    }
    override def multiply(m: Array[N], c: C) = {
      val (p, e) = x
      (p.multiply(m, c), e + m.degree)
    }
    def map(f: (Array[N], C) => (Array[N], C)) = {
      val (p, e) = x
      (p.map(f), e)
    }
  }
  override def gb(xs: Element[T]*) = new SugarEngine(using this).process(xs)
}

object PolynomialWithSugar {
  type Element[T] = (T, BigInteger)
}
