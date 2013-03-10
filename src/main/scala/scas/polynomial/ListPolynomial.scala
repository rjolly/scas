package scas.polynomial

import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer
import scas.Implicits.{infixRingOps, infixOrderingOps}
import ListPolynomial.Element

trait ListPolynomial[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends Polynomial[T, C, N] {
  override def isZero(x: T) = x.value.isEmpty
  def apply(s: (Array[N], C)*) = apply(List(s: _*))
  def apply(value: List[(Array[N], C)]): T

  def iterator(x: T) = x.value.iterator

  def iterator(x: T, m: Array[N]) = x.value.dropWhile({ r =>
    val (s, _) = r
    s > m
  }).iterator

  def reverseIterator(x: T) = x.value.reverseIterator

  def size(x: T) = x.value.size

  def head(x: T) = x.value.head

  def last(x: T) = x.value.last

  def combine(x: T, y: T, f: (C, C) => C) = {
    val res = new ListBuffer[(Array[N], C)]
    var leftx = x.value
    var lefty = y.value
    while (!leftx.isEmpty && !lefty.isEmpty) {
      val (s, a) = leftx.head
      val (t, b) = lefty.head
      if (s > t) {
        res += leftx.head
        leftx = leftx.tail
      } else if (s < t) {
        res += lefty.head
        lefty = lefty.tail
      } else {
        val c = f(a, b)
        if (!c.isZero) res += ((s, c))
        leftx = leftx.tail
        lefty = lefty.tail
      }
    }
    res ++= leftx
    res ++= lefty
    apply(res.toList)
  }

  def map(x: T, f: (Array[N], C) => (Array[N], C)) = {
    val res = new ListBuffer[(Array[N], C)]
    var left = x.value
    while (!left.isEmpty) {
      val (s, a) = left.head
      val (m, c) = f(s, a)
      if (!c.isZero) res += ((m, c))
      left = left.tail
    }
    apply(res.toList)
  }

  def sort(x: T) = apply(x.value.sortBy({ r =>
    val (s, _) = r
    s
  })(pp.ordering.reverse))
}

object ListPolynomial {
  trait Element[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends Polynomial.Element[T, C, N] { this: T =>
    val factory: ListPolynomial[T, C, N]
    val value: List[(Array[N], C)]
  }
}
