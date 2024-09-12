package scas.scripting

import scala.collection.immutable.{Map, SortedMap}
import scas.util.{Conversion, unary_~}
import scas.structure.ordered.Ring
import scas.structure.Monoid
import scas.math.Numeric
import Factors.Element

class Factors[T, N](using ring: Ring[T], numeric: Numeric[N]) extends Monoid[Element[T, N]] {
  def empty = SortedMap.empty[T, N]
  val one = empty
  def apply(x: T): Element[T, N] = if (x.isOne) empty else if (x.signum < 0) apply(-x) + ((-ring.one, numeric.one)) else empty + ((x, numeric.one))
  extension (x: Element[T, N]) override def isOne = x.isEmpty
  extension (x: Element[T, N]) def isUnit = if (x.isOne) true else {
    val (a, b) = x.head
    a.isUnit
  }
  extension (x: Element[T, N]) def multiply(y: Element[T, N]) = y.foldLeft(x)((l, r) => {
    val (a, b) = r
    val c = l.getOrElse(a, numeric.zero)
    if (a >< -ring.one && b >< numeric.one && c >< numeric.one) l - a else l + ((a, c + b))
  })
  def equiv(x: Element[T, N], y: Element[T, N]) = {
    val xs = x.iterator
    val ys = y.iterator
    while (xs.hasNext && ys.hasNext) {
      val (a, b) = xs.next
      val (c, d) = ys.next
      if (a <> c) return false
      else if (b <> d) return false
    }
    !xs.hasNext && !ys.hasNext
  }
  extension (x: Element[T, N]) def toCode(level: Level) = {
    var s = ring.one.show
    val p = if (x.size == 1) level else Level.Multiplication
    var m = 0
    for ((a, b) <- x) {
      val t = if (b >< numeric.one) a.toCode(p) else s"${a.toCode(Level.Power)}\\$b"
      s = if (m == 0) t else s + "*" + t
      m += 1
    }
    s
  }
  override def toString = s"Product($ring)"
  extension (x: Element[T, N]) def toMathML = {
    var s = ring.one.toMathML
    var m = 0
    for ((a, b) <- x) {
      val t = if (b >< numeric.one) a.toMathML else s"<apply><power/>${a.toMathML}<cn>$b</cn></apply>"
      s = if (m == 0) t else s"<apply><times/>$s$t</apply>"
      m += 1
    }
    s
  }
  def toMathML = s"<apply><cartesianproduct/>${ring.toMathML}</apply>"

  given ring2factors[S: Conversion[T]]: (S => Element[T, N]) = x => this(~x)
}

object Factors {
  type Element[T, N] = Map[T, N]
}
