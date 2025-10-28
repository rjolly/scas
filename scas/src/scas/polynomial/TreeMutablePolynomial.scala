package scas.polynomial

import language.experimental.{captureChecking, separationChecking}
import scala.jdk.CollectionConverters.MapHasAsScala
import TreePolynomial.Element

trait TreeMutablePolynomial[C, M] extends TreePolynomial[C, M] {
  extension (x: Element[C, M]) {
    override def subtract(y: Element[C, M]) = unmodifiable(super.subtract(modifiable(x))(y))

    override def multiply(y: Element[C, M]) = {
      val r = modifiable(zero)
      for (a, b) <- y.asScala do r.subtract(a, -b, x)
      unmodifiable(r)
    }

    override def reduce(ys: Element[C, M]*) = unmodifiable(super.reduce(modifiable(x))(ys*))

    override def reduce(strict: Boolean, tail: Boolean, ys: Element[C, M]*) = unmodifiable(super.reduce(modifiable(x))(strict, tail, ys*))

    override def subtract(m: M, c: C, y: Element[C, M]) = {
      val ys = y.entrySet.iterator
      while ys.hasNext do {
        val sa = ys.next
        val s = sa.getKey
        val a = sa.getValue
        val ac = a * c
        if !ac.isZero then {
          val sm = s * m
          val cc = x.getOrElse(sm, ring.zero) - ac
          if cc.isZero then x.remove(sm) else x.put(sm, cc)
        }
      }
      x
    }

    override def multiplyRight(c: C) = {
      val xs = x.entrySet.iterator
      while xs.hasNext do {
        val sa = xs.next
        val s = sa.getKey
        val a = sa.getValue
        val ac = a * c
        if !ac.isZero then sa.setValue(ac)
        else xs.remove
      }
      x
    }
  }
}
