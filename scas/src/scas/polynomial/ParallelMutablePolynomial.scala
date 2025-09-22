package scas.polynomial

import scala.collection.parallel.CollectionConverters.*

trait ParallelMutablePolynomial[T, C, M] extends Polynomial[T, C, M] {
  def unmodifiable(x: T): T
  def modifiable(x: T): T
  extension (x: T) override def multiply(y: T) = unmodifiable(y.toSeq.par.aggregate(() => modifiable(zero))({ (l, r) =>
    val (a, b) = r
    val k = l().subtract(a, -b, x)
    () => k
  }, { (a, b) =>
    val s = a().subtract(pp.one, -ring.one, b())
    () => s
  })())
}
