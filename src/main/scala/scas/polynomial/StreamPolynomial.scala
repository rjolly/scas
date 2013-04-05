package scas.polynomial

import scas.concurrent._
import scas.immutable.Stream
import scas.Implicits.{infixOrderingOps, infixRingOps}
import ExecutionContext.Implicits.global
import Stream.{#::, ConsWrapper, Empty}
import StreamPolynomial.Element

trait StreamPolynomial[T <: Element[T, C, N], C, N] extends IterablePolynomial[T, C, N] {
  type S = Stream[(Array[N], C)]
  def apply(s: (Array[N], C)*) = apply(Stream(s: _*))
  def apply(value: S): T

  def plus(x: T, y: T) = apply(plus(x.value, y.value))

  def plus(x: S, y: S): S = x match {
    case (s, a)#::tailx => y match {
      case (t, b)#::taily => {
        if (s > t) (s, a)#::tailx.map(plus(_, y))
        else if (s < t) (t, b)#::taily.map(plus(x, _))
        else {
          val c = a + b
          val result = (s, c)#::(for (sx <- tailx; sy <- taily) yield plus(sx, sy))
          if (!c.isZero) result else result.tail
        }
      }
      case Empty => x
    }
    case Empty => y
  }

  override def toSeq(x: T) = x.value

  def map(x: T, f: (Array[N], C) => (Array[N], C)) = apply(map(x.value, f))

  def map(x: S, f: (Array[N], C) => (Array[N], C)): S = x match {
    case (s, a)#::tail => {
      val (m, c) = f(s, a)
      val result = (m, c)#::tail.map(map(_, f))
      if (!c.isZero) result else result.tail
    }
    case Empty => Empty
  }
}

object StreamPolynomial {
  trait Element[T <: Element[T, C, N], C, N] extends IterablePolynomial.Element[T, C, N] { this: T =>
    val factory: StreamPolynomial[T, C, N]
    val value: Stream[(Array[N], C)]
  }
}
