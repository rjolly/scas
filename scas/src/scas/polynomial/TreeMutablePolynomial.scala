package scas.polynomial

import java.util.TreeMap
import scala.collection.JavaConverters.mapAsScalaMapConverter
import scala.annotation.targetName
import scas.structure.Ring
import scas.power.PowerProduct
import TreePolynomial.Element

trait TreeMutablePolynomial[C, M](using ring: Ring[C], pp: PowerProduct[M]) extends TreePolynomial[C, M] {
  extension (x: Element[C, M]) override def subtract(y: Element[C, M]) = new TreeMap(x).subtract(pp.one, ring.one, y)

  extension (x: Element[C, M]) override def multiply(y: Element[C, M]) = {
    val r = new TreeMap(zero)
    for ((a, b) <- y.asScala) r.subtract(a, -b, x)
    unmodifiable(r)
  }

  extension (x: Element[C, M]) override def subtract(m: M, c: C, y: Element[C, M]) = {
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

  extension (x: Element[C, M]) @targetName("coefMultiply") override def multiply(c: C) = {
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
