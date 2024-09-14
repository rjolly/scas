package scas.polynomial

import scala.jdk.CollectionConverters.MapHasAsScala
import TreePolynomial.Element

abstract class TreeMutablePolynomial[C, M] extends TreePolynomial[C, M] {
  extension (x: Element[C, M]) {
    override def subtract(y: Element[C, M]) = unmodifiable(super.subtract(modifiable(x))(y))

    override def multiply(y: Element[C, M]) = {
      val r = modifiable(zero)
      for ((a, b) <- y.asScala) r.subtract(a, -b, x)
      unmodifiable(r)
    }

    override def reduce(ys: Element[C, M]*) = unmodifiable(super.reduce(modifiable(x))(ys*))

    override def reduce(remainder: Boolean, tail: Boolean, ys: Element[C, M]*) = unmodifiable(super.reduce(modifiable(x))(remainder, tail, ys*))

    override def subtract(m: M, c: C, y: Element[C, M]) = {
      val ys = y.entrySet.iterator
      while (ys.hasNext) {
        val sa = ys.next
        val s = sa.getKey
        val a = sa.getValue
        val ac = a * c
        if (!ac.isZero) {
          val sm = s * m
          val cc = x.getOrElse(sm, ring.zero) - ac
          if (cc.isZero) x.remove(sm) else x.put(sm, cc)
        }
      }
      x
    }

    override def multiplyRight(c: C) = {
      val xs = x.entrySet.iterator
      while (xs.hasNext) {
        val sa = xs.next
        val s = sa.getKey
        val a = sa.getValue
        val ac = a * c
        if (!ac.isZero) sa.setValue(ac)
        else xs.remove
      }
      x
    }
  }
}
