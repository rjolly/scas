package scas.polynomial

import scala.collection.mutable.ListBuffer
import ListPolynomial.Element

trait ListPolynomial[C, M] extends Polynomial[Element[C, M], C, M] {
  def apply(s: (M, C)*) = List(s*)

  extension (x: Element[C, M]) {
    def add(y: Element[C, M]) = {
      val res = new ListBuffer[(M, C)]
      var leftx = x
      var lefty = y
      while !leftx.isEmpty && !lefty.isEmpty do {
        val (s, a) = leftx.head
        val (t, b) = lefty.head
        if s > t then {
          res += leftx.head
          leftx = leftx.tail
        } else if s < t then {
          res += lefty.head
          lefty = lefty.tail
        } else {
          val c = a + b
          if !c.isZero then res += ((s, c))
          leftx = leftx.tail
          lefty = lefty.tail
        }
      }
      res ++= leftx
      res ++= lefty
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
      while !left.isEmpty do {
        val (s, a) = left.head
        val (m, c) = f(s, a)
        if !c.isZero then res += ((m, c))
        left = left.tail
      }
      res.toList
    }
  }
}

object ListPolynomial {
  type Element[C, M] = List[(M, C)]
}
