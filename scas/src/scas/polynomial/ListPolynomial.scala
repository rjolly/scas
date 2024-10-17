package scas.polynomial

import scala.collection.mutable.ListBuffer
import ListPolynomial.Element

trait ListPolynomial[C, M] extends Polynomial[Element[C, M], C, M] {
  def apply(s: (M, C)*) = List(s*)

  extension (x: Element[C, M]) {
    def add(y: Element[C, M]) = x.subtract(pp.one, -ring.one, y)

    override def subtract(m: M, c: C, y: Element[C, M]) = {
      val res = new ListBuffer[(M, C)]
      var leftx = x
      var lefty = y
      while (!leftx.isEmpty && !lefty.isEmpty) {
        val (s, a) = leftx.head
        val (t, b) = lefty.head
        val u = t * m
        if (s > u) {
          res += leftx.head
          leftx = leftx.tail
        } else if (s < u) {
          val d = b * c
          if (!d.isZero) res += ((u, -d))
          lefty = lefty.tail
        } else {
          val d = b * c
          val e = a - d
          if (!e.isZero) res += ((s, e))
          leftx = leftx.tail
          lefty = lefty.tail
        }
      }
      res ++= leftx
      res ++= lefty.multiply(m, -c)
      res.toList
    }

    override def isZero = x.isEmpty

    def iterator = x.iterator

    override def toSeq = x

    def size = x.size

    def head = x.head

    def last = x.last

    def map(f: (M, C) => (M, C)) = {
      val res = new ListBuffer[(M, C)]
      var left = x
      while (!left.isEmpty) {
        val (s, a) = left.head
        val (m, c) = f(s, a)
        if (!c.isZero) res += ((m, c))
        left = left.tail
      }
      res.toList
    }
  }
}

object ListPolynomial {
  type Element[C, M] = List[(M, C)]
}
