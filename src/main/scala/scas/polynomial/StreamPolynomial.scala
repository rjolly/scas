package scas.polynomial

import scas.concurrent._
import scas.immutable.Stream
import scas.Implicits.{infixRingOps, infixOrderingOps, infixPowerProductOps}
import ExecutionContext.Implicits.global
import Stream.{#::, ConsWrapper, Empty}
import StreamPolynomial.Element

trait StreamPolynomial[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends Polynomial[T, C, N] {
  type S = Stream[(Array[N], C)]

  override def isZero(x: T) = x.value.isEmpty
  def apply(s: (Array[N], C)*) = apply(Stream(s: _*))
  def apply(value: S): T

  override def add(x: T, m: Array[N], c: C, y: T) = apply(add(x.value, m, c, y.value))

  def add(x: S, m: Array[N], c: C, y: S): S = x match {
    case (s, a)#::tailx => y match {
      case (t, b)#::taily => {
        val (tm, bc) = (t * m, b * c)
        if (s > tm) (s, a)#::tailx.map(add(_, m, c, y))
        else if (s < tm) (tm, bc)#::taily.map(add(x, m, c, _))
        else {
          val cc = a + bc
          val result = (s, cc)#::(for (sx <- tailx; sy <- taily) yield add(sx, m, c, sy))
          if (!cc.isZero) result else result.tail
        }
      }
      case Empty => x
    }
    case Empty => y match {
      case (t, b)#::taily => {
        val (tm, bc) = (t * m, b * c)
        (tm, bc)#::taily.map(add(x, m, c, _))
      }
      case Empty => Empty
    }
  }

  def iterator(x: T) = x.value.iterator

  def iterator(x: T, m: Array[N]) = x.value.dropWhile({ r =>
    val (s, _) = r
    s > m
  }).iterator

  def reverseIterator(x: T) = x.value.reverseIterator

  def size(x: T) = x.value.size

  def head(x: T) = x.value.head

  def last(x: T) = x.value.last

  def combine(x: T, y: T, f: (C, C) => C) = apply(combine(x.value, y.value, f))

  def combine(x: S, y: S, f: (C, C) => C): S = x match {
    case (s, a)#::tailx => y match {
      case (t, b)#::taily => {
        if (s > t) (s, a)#::tailx.map(combine(_, y, f))
        else if (s < t) (t, b)#::taily.map(combine(x, _, f))
        else {
          val cc = a + b
          val result = (s, cc)#::(for (sx <- tailx; sy <- taily) yield combine(sx, sy, f))
          if (!cc.isZero) result else result.tail
        }
      }
      case Empty => x
    }
    case Empty => y
  }

  def map(x: T, f: (Array[N], C) => (Array[N], C)) = apply(map(x.value, f))

  def map(x: S, f: (Array[N], C) => (Array[N], C)): S = x match {
    case (s, a)#::tail => {
      val (m, c) = f(s, a)
      val result = (m, c)#::tail.map(map(_, f))
      if (!c.isZero) result else result.tail
    }
    case Empty => Empty
  }

  def sort(x: T) = apply(x.value.sortBy({ r =>
    val (s, _) = r
    s
  })(pp.ordering.reverse))
}

object StreamPolynomial {
  trait Element[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends Polynomial.Element[T, C, N] { this: T =>
    val factory: StreamPolynomial[T, C, N]
    val value: Stream[(Array[N], C)]
  }
}
