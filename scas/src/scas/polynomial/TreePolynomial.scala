package scas.polynomial

import java.util.{SortedMap, TreeMap, Collections}
import scala.collection.JavaConverters.mapAsScalaMapConverter
import scas.structure.Ring
import scas.power.PowerProduct
import TreePolynomial.Element

class TreePolynomial[C, M](using ring: Ring[C], pp: PowerProduct[M]) extends Polynomial[Element[C, M], C, M] {
  override def apply(x: Element[C, M]) = Collections.unmodifiableSortedMap(x)
  def apply(s: (M, C)*) = {
    val r = new TreeMap[M, C](pp.reverse)
    for ((m, c) <- s) r.put(m, c)
    this(r)
  }

  extension (x: Element[C, M]) def add(y: Element[C, M]) = {
    val r = new TreeMap(x)
    for ((t, b) <- y.asScala) {
      val c = r.getOrElse(t, ring.zero) + b
      if (c.isZero) r.remove(t) else r.put(t, c)
    }
    this(r)
  }

  extension (x: Element[C, M]) override def subtract(y: Element[C, M]) = new TreeMap(x).subtract(pp.one, ring.one, y)

  extension (x: Element[C, M]) override def multiply(y: Element[C, M]) = {
    val r = new TreeMap(zero)
    for ((a, b) <- x.asScala) r.subtract(a, -b, y)
    this(r)
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
    val r = x
    val ys = iterator(y)
    while (ys.hasNext) {
      val (s, a) = ys.next
      val ac = a * c
      if (!ac.isZero) {
        val sm = s * m
        val cc = r.getOrElse(sm, ring.zero) - ac
        if (cc.isZero) r.remove(sm) else r.put(sm, cc)
      }
    }
    r
  }

  extension (x: Element[C, M]) def getOrElse(m: M, c: C) = {
    val a = x.get(m)
    if (a == null) c else a
  }

  extension (x: Element[C, M]) override def reduce(m: M, a: C, y: Element[C, M], b: C) = new TreeMap(super.multiply(x)(b)).subtract(m, a, y)

  extension (x: Element[C, M]) def map(f: (M, C) => (M, C)) = {
    val r = new TreeMap(zero)
    for ((s, a) <- x.asScala) {
      val (m, c) = f(s, a)
      if (!c.isZero) r.put(m, c)
    }
    this(r)
  }

  override def sort(x: Element[C, M]) = x
}

object TreePolynomial {
  type Element[C, M] = SortedMap[M, C]
}
