package scas.polynomial

import scala.collection.parallel.CollectionConverters.*

trait ParallelPolynomial[T, C, M] extends Polynomial[T, C, M] {
  extension (x: T) override def multiply(y: T) = y.toSeq.par.aggregate(zero)({ (l, r) =>
    val (a, b) = r
    l.subtract(a, -b, x)
  }, _ + _)
}
