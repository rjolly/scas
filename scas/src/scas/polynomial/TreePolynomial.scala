package scas.polynomial

import java.util.{SortedMap, TreeMap}
import scala.collection.JavaConverters.mapAsScalaMapConverter
import scas.structure.Ring
import scas.power.PowerProduct
import TreePolynomial.Element

class TreePolynomial[C : Ring, M : PowerProduct] extends Polynomial[Element[C, M], C, M] {
  def apply(s: (M, C)*) = {
    val r = new TreeMap[M, C](pp.reverse)
    for ((m, c) <- s) r.put(m, c)
    r
  }

  def (x: Element[C, M]) + (y: Element[C, M]) = {
    val r = this(x.toSeq: _*)
    for ((t, b) <- y.asScala) {
      val c = r.getOrElse(t, ring.zero) + b
      if (c >< ring.zero) r.remove(t) else r.put(t, c)
    }
    r
  }

  override def (x: Element[C, M]) - (y: Element[C, M]) = new TreeMap(x).subtract(pp.one, ring.one, y)

  def iterator(x: Element[C, M]) = x.asScala.iterator

  override def (x: Element[C, M]).iterator(m: M) = x.tailMap(m).asScala.iterator

  override def (x: Element[C, M]).toSeq = x.asScala.toSeq

  def size(x: Element[C, M]) = x.size

  def head(x: Element[C, M]) = x.asScala.head

  def last(x: Element[C, M]) = x.asScala.last

  override def (x: Element[C, M]).coefficient(m: M) = x.getOrElse(m, ring.zero)

  override def (x: Element[C, M]).subtract(m: M, c: C, y: Element[C, M]) = {
    val r = x
    for ((s, a) <- y.asScala) {
      val ac = a * c
      if (ac <> ring.zero) {
        val sm = s * m
        val cc = r.getOrElse(sm, ring.zero) - ac
        if (cc >< ring.zero) r.remove(sm) else r.put(sm, cc)
      }
    }
    r
  }

  def (x: Element[C, M]).getOrElse(m: M, c: C) = {
    val a = x.get(m)
    if (a == null) c else a
  }

  def (x: Element[C, M]).map(f: (M, C) => (M, C)) = {
    val r = this()
    for ((s, a) <- x.asScala) {
      val (m, c) = f(s, a)
      if (c <> ring.zero) r.put(m, c)
    }
    r
  }

  override def sort(x: Element[C, M]) = x
}

object TreePolynomial {
  type Element[C, M] = SortedMap[M, C]
  def apply[C, M](using TreePolynomial[C, M]) = summon[TreePolynomial[C, M]]
  given coef2poly[D, C, M](using Conversion[D, C], TreePolynomial[C, M]) as Conversion[D, Element[C, M]] = TreePolynomial[C, M].fromRing(_)
}
