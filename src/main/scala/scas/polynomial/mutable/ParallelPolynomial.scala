package scas.polynomial.mutable

import scas.polynomial.Polynomial
import scas.Implicits.infixRingOps
import Polynomial.Element

trait ParallelPolynomial[T <: Element[T, C, N], C, N] extends Polynomial[T, C, N] {
  override def times(x: T, y: T) = toSeq(y).par.aggregate(() => zero)({ (l, r) =>
    val (a, b) = r
    val k = subtract(l(), a, -b, x)
    () => k
  }, { (a, b) =>
    val s = a() + b()
    () => s
  })()
}
