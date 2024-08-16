package scas.polynomial

import scala.reflect.ClassTag
import scas.module.ArrayModule
import PolynomialWithRepr.Element

abstract class PolynomialWithRepr[T : ClassTag, C, M](dimension: Int) extends Polynomial[Element[T], C, M] {
  given factory: Polynomial[T, C, M]
  given module: ArrayModule[T] = ArrayModule(factory)(dimension)
  def apply(p: T, n: Int) = (p, module.generator(n))
  def apply(s: (M, C)*) = this(factory(s*))
  def apply(p: T) = (p, module.zero)
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
      (p + q, e + f)
    }
    override def ppMultiplyRight(m: M) = {
      val (p, e) = x
      (p.ppMultiplyRight(m), e%* factory(m))
    }
    override def multiply(m: M, c: C) = {
      val (p, e) = x
      (p.multiply(m, c), e%* factory(m, c))
    }
    override def multiplyRight(c: C) = {
      val (p, e) = x
      (p%* c, e%* factory(c))
    }
    def map(f: (M, C) => (M, C)) = {
      val (p, e) = x
      (p.map(f), e)
    }
  }
}

object PolynomialWithRepr {
  type Element[T] = (T, Array[T])
}
