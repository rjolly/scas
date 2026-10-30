package scas.polynomial.binary

import scala.jdk.CollectionConverters.MapHasAsScala
import TreePolynomial.Element

trait TreePolynomial[C] extends Polynomial[Element[C], C] with scas.polynomial.TreePolynomial[C, Array[Int]] {
  extension (x: Element[C]) {
    override def map(f: (Array[Int], C) => (Array[Int], C)) = {
      val r = modifiable(zero)
      for (s, a) <- x.asScala do {
        val (m, c) = f(s, a)
        val d = r.getOrElse(m, ring.zero) + c
        if d.isZero then r.remove(m) else r.put(m, d)
      }
      unmodifiable(r)
    }
  }
}

object TreePolynomial {
  type Element[C] = scas.polynomial.TreePolynomial.Element[C, Array[Int]]
}
