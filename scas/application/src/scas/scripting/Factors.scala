package scas.scripting

import scas.structure.{Ring, Module}
import scas.math.Numeric
import scas.base.BigInteger
import BigInteger.given
import Factors.Element

abstract class Factors[T, N](using val ring: Ring[T], numeric: Numeric[N]) extends Module[Element[T, N], T] {
  def empty: Element[T, N]
  override val zero = empty + ((ring.zero, numeric.one))
  extension (x: Element[T, N]) {
    override def convert = x.map((a, b) => (a.convert, b))
    override def isZero = x.getOrElse(ring.zero, numeric.zero) >< numeric.one
    def add(y: Element[T, N]) = {
      val (a, b) = x.partition((c, _) => y.contains(c))
      val (_, d) = y.partition((c, _) => a.contains(c))
      a%* (b.expand + d.expand)
    }
    def expand = x.foldLeft(ring.one) { (l, r) =>
      val (a, b) = r
      l * a \ b.toLong
    }
    def subtract(y: Element[T, N]) = x + (-y)
    def multiplyRight(c: T) = if (x.isZero || c.isZero) zero else if (c.isOne) x else {
      val d = x.getOrElse(c, numeric.zero)
      x + ((c, d + numeric.one))
    }
    override def unary_- = x%* (-ring.one)
    def signum = if (x.isZero) 0 else if(x.getOrElse(-ring.one, numeric.zero) >< numeric.one) -1 else 1
  }
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
  extension (c: T) def multiplyLeft(x: Element[T, N]) = x%* c

  extension (ring: Ring[T]) def pow(n: Int) = {
    assert (n == 0)
    this
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
  override def toString = s"$ring.pow(0)"
  def toMathML = s"<msup>${ring.toMathML}<cn>0</cn></msup>"
}

object Factors {
  type Element[T, N] = Map[T, N]
}
