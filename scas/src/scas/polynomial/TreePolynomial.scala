package scas.polynomial

import scas.util.{SortedMap, TreeMap}
import language.experimental.{captureChecking, separationChecking}
import TreePolynomial.Element

trait TreePolynomial[C, M] extends Polynomial[Element[C, M], C, M] {
  def modifiable(x: Element[C, M]): Element[C, M]^ = new TreeMap(x)
  def apply(s: (M, C)*) = {
    val r = new TreeMap[M, C](pp.reverse)
    for (m, c) <- s do r._put(m, c)
    r
  }

  extension (x: Element[C, M]) def add(y: Element[C, M]) = {
    val r = modifiable(x)
    for (t, b) <- y.asScala do {
      val c = r.getOrElse(t, ring.zero) + b
      if c.isZero then r._remove(t) else r._put(t, c)
    }
    r
  }

  extension (x: Element[C, M]) {
    override def isZero = x.isEmpty

    def iterator = x.asScala.iterator

    override def iterator(m: M) = x.tailMapAsScala(m).iterator

    override def toSeq = x.asScala.toSeq

    def size = x.size

    def head = x.asScala.head

    def last = x.asScala.last

    override def coefficient(m: M) = x.getOrElse(m, ring.zero)

    def getOrElse(m: M, c: C) = {
      val a = x.get(m)
      if a == null then c else a
    }

    def map(f: (M, C) => (M, C)) = {
      val r = modifiable(zero)
      for (s, a) <- x.asScala do {
        val (m, c) = f(s, a)
        if !c.isZero then r._put(m, c)
      }
      r
    }
  }

  extension (consume x: Element[C, M]) {
    override def sort = x
  }
}

object TreePolynomial {
  type Element[C, M] = SortedMap[M, C]
}
