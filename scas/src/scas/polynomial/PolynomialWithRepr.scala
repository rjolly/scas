package scas.polynomial

import scala.reflect.ClassTag
import scas.module.ArrayModule
import scas.structure.Ring
import scas.power.PowerProduct
import PolynomialWithRepr.Element

class PolynomialWithRepr[T : ClassTag, C : Ring, M : PowerProduct](using factory: Polynomial[T, C, M])(dimension: Int) extends Polynomial[Element[T], C, M] {
  given module: ArrayModule[T] = new ArrayModule[T](dimension)
  def apply(p: T, n: Int) = (p, module.generator(n))
  def apply(s: (M, C)*) = this(factory(s*))
  def apply(p: T) = (p, module.zero)
  def iterator(x: Element[T]) = {
    val (p, _) = x
    factory.iterator(p)
  }
  def size(x: Element[T]) = {
    val (p, _) = x
    factory.size(p)
  }
  def head(x: Element[T]) = {
    val (p, _) = x
    factory.head(p)
  }
  def last(x: Element[T]) = {
    val (p, _) = x
    factory.last(p)
  }
  extension (x: Element[T]) {
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
