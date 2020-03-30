package scas.polynomial

import java.util.{SortedMap, TreeMap}
import scala.collection.JavaConverters.mapAsScalaMapConverter
import scas.structure.Ring
import scas.power.PowerProduct
import TreePolynomial.Element

class TreePolynomial[C : Ring, N : PowerProduct] extends Polynomial[Element[C, N], C, N] {
  def apply(s: (Array[N], C)*) = {
    val r = new TreeMap[Array[N], C](pp.reverse)
    s.foreach((m, c) => r.put(m, c))
    r
  }

  def (x: Element[C, N]) + (y: Element[C, N]) = {
    val r = new TreeMap(x)
    y.asScala.foreach { (t, b) =>
      val c = r.asScala.getOrElse(t, ring.zero) + b
      if (c >< ring.zero) r.remove(t) else r.put(t, c)
    }
    r
  }

  def iterator(x: Element[C, N]) = x.asScala.iterator

  override def (x: Element[C, N]).iterator(m: Array[N]) = x.tailMap(m).asScala.iterator

  override def toSeq(x: Element[C, N]) = x.asScala.toSeq

  def size(x: Element[C, N]) = x.size

  def head(x: Element[C, N]) = x.asScala.head

  def last(x: Element[C, N]) = x.asScala.last

  override def (x: Element[C, N]).coefficient(m: Array[N]) = x.asScala.getOrElse(m, ring.zero)

  override def (x: Element[C, N]).subtract(m: Array[N], c: C, y: Element[C, N]) = {
    val r = new TreeMap(x)
    y.asScala.foreach { (s, a) =>
      val (sm, ac) = (s * m, a * c)
      val cc = r.asScala.getOrElse(sm, ring.zero) - ac
      if (cc >< ring.zero) r.remove(sm) else r.put(sm, cc)
    }
    r
  }

  def (x: Element[C, N]).map(f: (Array[N], C) => (Array[N], C)) = {
    val r = apply()
    x.asScala.foreach { (s, a) =>
      val (m, c) = f(s, a)
      if (c >< ring.zero) {} else r.put(m, c)
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
