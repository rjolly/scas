package scas.polynomial

import java.util.{SortedMap, TreeMap}
import scala.collection.JavaConverters.mapAsScalaMapConverter
import scas.structure.Ring
import scas.power.PowerProduct
import TreePolynomial.Element

class TreePolynomial[C : Ring, N : PowerProduct] extends Polynomial[Element[C, N], C, N] {
  def apply(s: (Array[N], C)*) = {
    val r = new TreeMap[Array[N], C](pp.reverse)
    for ((m, c) <- s) r.put(m, c)
    r
  }

  def (x: Element[C, N]) + (y: Element[C, N]) = {
    val r = this(x.toSeq: _*)
    for ((t, b) <- y.asScala) {
      val c = r.getOrElse(t, ring.zero) + b
      if (c >< ring.zero) r.remove(t) else r.put(t, c)
    }
    r
  }

  override def (x: Element[C, N]) - (y: Element[C, N]) = new TreeMap(x).subtract(pp.one, ring.one, y)

  def iterator(x: Element[C, N]) = x.asScala.iterator

  override def (x: Element[C, N]).iterator(m: Array[N]) = x.tailMap(m).asScala.iterator

  override def (x: Element[C, N]).toSeq = x.asScala.toSeq

  def size(x: Element[C, N]) = x.size

  def head(x: Element[C, N]) = x.asScala.head

  def last(x: Element[C, N]) = x.asScala.last

  override def (x: Element[C, N]).coefficient(m: Array[N]) = x.getOrElse(m, ring.zero)

  override def (x: Element[C, N]).subtract(m: Array[N], c: C, y: Element[C, N]) = {
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

  def (x: Element[C, N]).getOrElse(m: Array[N], c: C) = {
    val a = x.get(m)
    if (a == null) c else a
  }

  def (x: Element[C, N]).map(f: (Array[N], C) => (Array[N], C)) = {
    val r = this()
    for ((s, a) <- x.asScala) {
      val (m, c) = f(s, a)
      if (c <> ring.zero) r.put(m, c)
    }
    r
  }

  override def sort(x: Element[C, N]) = x
}

object TreePolynomial {
  type Element[C, N] = SortedMap[Array[N], C]
  def apply[C, N](using TreePolynomial[C, N]) = summon[TreePolynomial[C, N]]
  given coef2poly[D, C, N](using Conversion[D, C], TreePolynomial[C, N]) as Conversion[D, Element[C, N]] = TreePolynomial[C, N].fromRing(_)
}
