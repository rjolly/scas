package scas.polynomial

import scala.concurrent._
import scala.annotation.tailrec
import scas.immutable.Stream
import scas.Implicits.{infixRingOps, infixOrderingOps, infixPowerProductOps}
import ExecutionContext.Implicits.global
import Stream.{#::, ConsWrapper, Empty}
import ListPolynomial.Element

trait StreamPolynomial[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends ListPolynomial[T, C, N] {
  type Term = Stream[(Array[N], C)]

  override def times(x: T, y: T) = apply(multiply(Stream(zero.value.toStream), Stream(x.value.toStream), Stream(y.value.toStream)).toList)

  def multiply(zero: Term, x: Term, y: Term): Term = (zero /: y) { (l, r) =>
    val (a, b) = r
    add(x, a, b, l)
  }

  def add(x: Term, m: Array[N], c: C, y: Term): Term = x match {
    case (s, a)#::tailx => y match {
      case (t, b)#::taily => {
        val (sm, ac) = (s * m, a * c)
        if (sm > t) (sm, ac)#::tailx.map(add(_, m, c, y))
        else if (sm < t) (t, b)#::taily.map(add(x, m, c, _))
        else {
          val cc = ac + b
          val result = (sm, cc)#::(for (sx <- tailx; sy <- taily) yield add(sx, m, c, sy))
          if (!cc.isZero) result else result.tail
        }
      }
      case Empty => {
        val (sm, ac) = (s * m, a * c)
        (sm, ac)#::tailx.map(add(_, m, c, y))
      }
    }
    case Empty => y
  }
}
