package scas.polynomial

import scala.collection.mutable.ListBuffer
import scas.Implicits.{infixRingOps, infixOrderingOps}
import ListPolynomial.Element

trait ListPolynomial[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends IterablePolynomial[T, C, N] {
  def apply(s: (Array[N], C)*) = apply(List(s: _*))
  def apply(value: List[(Array[N], C)]): T

  override def reverseIterator(x: T) = x.value.reverseIterator

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
  trait Element[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends IterablePolynomial.Element[T, C, N] { this: T =>
    val factory: ListPolynomial[T, C, N]
    val value: List[(Array[N], C)]
  }
}
