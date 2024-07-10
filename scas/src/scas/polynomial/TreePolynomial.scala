package scas.polynomial

import java.util.{SortedMap, TreeMap, Collections}
import scala.jdk.CollectionConverters.MapHasAsScala
import TreePolynomial.Element

abstract class TreePolynomial[C, M] extends Polynomial[Element[C, M], C, M] {
  def unmodifiable(x: Element[C, M]) = Collections.unmodifiableSortedMap(x)
  def modifiable(x: Element[C, M]) = new TreeMap(x)
  def apply(s: (M, C)*) = {
    val r = new TreeMap[M, C](pp.reverse)
    for ((m, c) <- s) r.put(m, c)
    unmodifiable(r)
  }

  extension (x: Element[C, M]) def add(y: Element[C, M]) = {
    val r = modifiable(x)
    for ((t, b) <- y.asScala) {
      val c = r.getOrElse(t, ring.zero) + b
      if (c.isZero) r.remove(t) else r.put(t, c)
    }
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

  extension (x: Element[C, M]) def getOrElse(m: M, c: C) = {
    val a = x.get(m)
    if (a == null) c else a
  }

  extension (x: Element[C, M]) def map(f: (M, C) => (M, C)) = {
    val r = modifiable(zero)
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
