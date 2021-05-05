package scas.polynomial

import java.util.{SortedMap, TreeMap, Collections}
import scala.collection.JavaConverters.mapAsScalaMapConverter
import scala.annotation.targetName
import scas.structure.Ring
import scas.power.PowerProduct
import TreePolynomial.Element

class TreePolynomial[C, M](using ring: Ring[C], pp: PowerProduct[M]) extends Polynomial[Element[C, M], C, M] {
  def unmodifiable(x: Element[C, M]) = Collections.unmodifiableSortedMap(x)
  def apply(s: (M, C)*) = {
    val r = new TreeMap[M, C](pp.reverse)
    for ((m, c) <- s) r.put(m, c)
    unmodifiable(r)
  }

  extension (x: Element[C, M]) def add(y: Element[C, M]) = {
    val r = new TreeMap(x)
    for ((t, b) <- y.asScala) {
      val c = r.getOrElse(t, ring.zero) + b
      if (c.isZero) r.remove(t) else r.put(t, c)
    }
    unmodifiable(r)
  }

  extension (x: Element[C, M]) override def subtract(y: Element[C, M]) = new TreeMap(x).subtract(pp.one, ring.one, y)

  extension (x: Element[C, M]) override def multiply(y: Element[C, M]) = {
    val r = new TreeMap(zero)
    for ((a, b) <- y.asScala) r.subtract(a, -b, x)
    unmodifiable(r)
  }

  def iterator(x: Element[C, M]) = x.asScala.iterator

  extension (x: Element[C, M]) {
    override def iterator(m: M) = x.tailMap(m).asScala.iterator

    override def toSeq = x.asScala.toSeq
  }

  def size(x: Element[C, M]) = x.size

  def head(x: Element[C, M]) = x.asScala.head

  def last(x: Element[C, M]) = x.asScala.last

  extension (x: Element[C, M]) override def coefficient(m: M) = x.getOrElse(m, ring.zero)

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

  extension (x: Element[C, M]) def getOrElse(m: M, c: C) = {
    val a = x.get(m)
    if (a == null) c else a
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

  extension (x: Element[C, M]) def map(f: (M, C) => (M, C)) = {
    val r = new TreeMap(zero)
    for ((s, a) <- x.asScala) {
      val (m, c) = f(s, a)
      if (!c.isZero) r.put(m, c)
    }
    unmodifiable(r)
  }

  override def sort(x: Element[C, M]) = x
}

object TreePolynomial {
  type Element[C, M] = SortedMap[M, C]
}
